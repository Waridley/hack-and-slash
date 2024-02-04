use crate::{
	mats::{DistanceDither, StdMatExt},
	nav::heightmap::FnsThatShouldBePub,
	offloading::{wasm_yield, Offload, OffloadedTask, TaskHandle, TaskOffloader},
	planet::{
		chunks::{
			ChunkCenter, ChunkIndex, LoadedChunks, CHUNK_COLS, CHUNK_ROWS, CHUNK_SCALE,
			TERRAIN_CELL_SIZE,
		},
		frame::Frame,
		seeds::PlanetSeed,
		terrain::noise::{ChooseAndSmooth, Source, SyncWorley},
		PlanetVec2,
	},
	player::player_entity::Root,
	util::{Diff, Factory, Spawnable},
};
use ::noise::{
	Add, Billow, Fbm, HybridMulti, MultiFractal, NoiseFn, Perlin, RidgedMulti, ScaleBias, Seedable,
	Value,
};
use bevy::{
	ecs::system::{EntityCommands, SystemParamItem},
	prelude::*,
	render::{
		batching::NoAutomaticBatching,
		mesh::{PrimitiveTopology, VertexAttributeValues::Float32x3},
	},
	utils::HashMap,
};
use bevy_rapier3d::{parry::shape::SharedShape, prelude::*};
use enum_components::ERef;
use rapier3d::{geometry::HeightField, na::DMatrix};
use std::{f32::consts::*, ops::DerefMut, sync::Arc};
use web_time::{Duration, Instant};

pub mod noise;

pub type Noise = ChooseAndSmooth<4>;

pub fn plugin(app: &mut App) -> &mut App {
	// WHY is there no length function on `Vector2`??
	let diameter = (CHUNK_SCALE.x * CHUNK_SCALE.x + CHUNK_SCALE.y * CHUNK_SCALE.y).sqrt();
	dbg!(diameter);
	app.add_systems(Startup, setup)
		.add_systems(PostStartup, spawn_boxes)
		.init_resource::<ChunkLoadingTasks>()
		.insert_resource(UnloadDistance(5.0 * diameter))
		.add_systems(Update, spawn_loaded_chunks)
		.add_systems(Last, (load_nearby_chunks, unload_distant_chunks))
}

pub fn setup(
	mut cmds: Commands,
	assets: Res<AssetServer>,
	mut chunk_loading_tasks: ResMut<ChunkLoadingTasks>,
	mut task_offloader: TaskOffloader,
) {
	let material = assets.load("shaders/terrain.mat.ron");

	cmds.insert_resource(TerrainMaterial(material.clone()));
	let seeds = PlanetSeed::default();
	info!(name: "seed", seed = %seeds);
	let seeds = seeds.tera;

	// Add is not Clone, so we'll use a closure to get multiple copies
	let base = || {
		Add::new(
			ScaleBias::new(
				HybridMulti::<Value>::default()
					.set_frequency(0.00032)
					.set_persistence(0.5),
			)
			.set_scale(2.0),
			Fbm::<Perlin>::default()
				.set_frequency(0.00128)
				.set_octaves(2)
				.set_persistence(0.45)
				.set_seed(seeds.base),
		)
	};

	let strength_noise = Fbm::<Perlin>::default()
		.set_frequency(0.00128)
		.set_octaves(2)
		.set_seed(seeds.perlin().strength());

	let perlin = Source::new(
		Add::new(
			ScaleBias::new(
				Fbm::<Perlin>::default()
					.set_frequency(0.0256)
					.set_persistence(0.45)
					.set_seed(seeds.perlin().heights()),
			)
			.set_scale(0.05),
			base(),
		),
		strength_noise.clone(),
		// Constant::new(-1.0),
	);

	let worley = Source::new(
		Add::new(
			ScaleBias::new(
				SyncWorley::default()
					.set_frequency(0.05)
					.set_seed(seeds.worley().heights()),
			)
			.set_scale(0.1),
			base(),
		),
		strength_noise.clone().set_seed(seeds.worley().strength()),
		// Constant::new(-1.0),
	);

	let billow = Source::new(
		Add::new(
			ScaleBias::new(
				Billow::<Perlin>::default()
					.set_frequency(0.02)
					.set_octaves(2)
					.set_seed(seeds.billow().heights()),
			)
			.set_scale(0.04),
			base(),
		),
		strength_noise.clone().set_seed(seeds.billow().strength()),
		// Constant::new(-1.0),
	);

	let ridged = Source::new(
		Add::new(
			ScaleBias::new(
				RidgedMulti::<Perlin>::default()
					.set_frequency(0.02)
					.set_seed(seeds.ridged().heights()),
			)
			.set_scale(0.07),
			base(),
		),
		strength_noise.clone().set_seed(seeds.ridged().strength()),
		// Constant::new(-1.0),
	);

	let noise = ChooseAndSmooth::new([perlin, worley, billow, ridged]);

	let noise = PlanetHeightSource::new(noise);

	// Spawn center first
	generate_chunk(
		ChunkIndex::new(0, 0),
		assets.clone(),
		noise.clone(),
		&mut *chunk_loading_tasks,
		&mut task_offloader,
	);

	cmds.insert_resource(noise);

	let mesh = Mesh::from(shape::Cube { size: 64.0 })
		// All of this makes cube meshes use the same compiled shader as heightmap terrain, preventing freezes
		.with_duplicated_vertices()
		.with_computed_flat_normals()
		.with_removed_attribute(Mesh::ATTRIBUTE_UV_0);
	let collider = Collider::cuboid(32.0, 32.0, 32.0);
	let mesh = assets.add(mesh);

	cmds.insert_resource(TerrainTemplate {
		mesh,
		material,
		collider,
	});
}

pub fn spawn_boxes(mut factory: Factory<TerrainObject>) {
	factory.spawn(Transform::from_translation(Vec3::new(0.0, 0.0, -48.0)));
	factory.spawn(Transform {
		translation: Vec3::new(-40.0, -8.0, -44.0),
		rotation: Quat::from_euler(EulerRot::ZXY, 0.0, 0.0, -FRAC_PI_3),
		..default()
	});
	factory.spawn(Transform {
		translation: Vec3::new(42.0, 24.0, -36.0),
		rotation: Quat::from_rotation_y(FRAC_PI_4),
		..default()
	});
	factory.spawn(Transform::from_translation(Vec3::new(-32.0, 42.0, -44.0)));
	factory.spawn(Transform::from_translation(Vec3::new(-64.0, 72.0, -32.0)));
	factory.spawn(Transform::from_translation(Vec3::new(-96.0, 0.0, -64.0)));
	factory.spawn(Transform {
		translation: Vec3::new(-24.0, 96.0, 16.0),
		rotation: Quat::from_rotation_x(FRAC_PI_3),
		..default()
	});
}

pub fn generate_chunk<'w, 's>(
	index: ChunkIndex,
	assets: AssetServer,
	noise: PlanetHeightSource,
	chunk_loading_tasks: &mut ChunkLoadingTasks,
	task_offloader: &mut TaskOffloader<'w, 's>,
) {
	let (columns, rows) = (CHUNK_COLS, CHUNK_ROWS);
	chunk_loading_tasks.insert(
		index,
		task_offloader.start(async move {
			let mut last_await = Instant::now();
			let heights = noise
				.generate_map(
					PlanetVec2::new(
						index.x as f64 * columns as f64,
						index.y as f64 * rows as f64,
					),
					rows,
					columns,
				)
				.await;
			let heights = HeightField::new(DMatrix::from_vec(rows, columns, heights), CHUNK_SCALE);

			fn face_normal(a: [f32; 3], b: [f32; 3], c: [f32; 3]) -> [f32; 3] {
				let (a, b, c) = (Vec3::from(a), Vec3::from(b), Vec3::from(c));
				(b - a).cross(c - a).normalize().into()
			}

			let mut vertices = Vec::with_capacity(heights.num_triangles() * 3);
			let mut normals = Vec::with_capacity(vertices.len());
			for tri in heights.triangles() {
				let tri = [
					[tri.a.x, tri.a.y, tri.a.z],
					[tri.b.x, tri.b.y, tri.b.z],
					[tri.c.x, tri.c.y, tri.c.z],
				];

				vertices.extend_from_slice(&tri);
				let normal = face_normal(tri[0], tri[1], tri[2]);
				normals.extend_from_slice(&[normal; 3]);

				if Instant::now().duration_since(last_await) > Duration::from_micros(500) {
					wasm_yield().await;
					last_await = Instant::now();
				}
			}

			let mut mesh = Mesh::new(PrimitiveTopology::TriangleList);
			mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, Float32x3(vertices));
			mesh.insert_attribute(Mesh::ATTRIBUTE_NORMAL, normals);

			let mesh = assets.add(mesh);
			(heights, mesh)
		}),
	);
}

/// Share mesh, material, and collider amongst multiple `TerrainObjects`
#[derive(Resource, Clone)]
pub struct TerrainTemplate {
	pub mesh: Handle<Mesh>,
	pub material: Handle<StdMatExt<DistanceDither>>,
	pub collider: Collider,
}

impl Spawnable for TerrainObject {
	type Params = Res<'static, TerrainTemplate>;
	type InstanceData = Transform;

	fn spawn<'w, 's, 'a>(
		cmds: &'a mut Commands<'w, 's>,
		params: &mut SystemParamItem<'w, 's, Self::Params>,
		transform: Transform,
	) -> EntityCommands<'w, 's, 'a> {
		cmds.spawn(TerrainObject {
			pbr: MaterialMeshBundle {
				mesh: params.mesh.clone(),
				material: params.material.clone(),
				transform,
				..default()
			},
			collider: params.collider.clone(),
			..default()
		})
	}
}

#[derive(Bundle)]
pub struct TerrainObject {
	pub pbr: MaterialMeshBundle<StdMatExt<DistanceDither>>,
	pub rigid_body: RigidBody,
	pub collider: Collider,
	pub restitution: Restitution,
	pub friction: Friction,
	pub ccd: Ccd,
}

impl Default for TerrainObject {
	fn default() -> Self {
		Self {
			pbr: MaterialMeshBundle::default(),
			rigid_body: RigidBody::Fixed,
			collider: Collider::default(),
			restitution: Restitution::new(0.5),
			friction: Friction::new(0.01),
			ccd: Ccd::enabled(),
		}
	}
}

#[derive(Component, Debug, Clone)]
pub struct Ground {
	pub heights: Arc<HeightField>,
}

#[derive(Resource, Deref, Clone)]
pub struct PlanetHeightSource {
	#[deref]
	pub noise: Arc<Noise>,
}

impl PlanetHeightSource {
	pub fn new(noise: impl Into<Arc<Noise>>) -> Self {
		Self {
			noise: noise.into(),
		}
	}
}

#[derive(Resource, Default, Deref, DerefMut)]
pub struct ChunkLoadingTasks(HashMap<ChunkIndex, TaskHandle<(HeightField, Handle<Mesh>)>>);

#[derive(Resource, Clone, Debug, Deref, DerefMut)]
pub struct TerrainMaterial(Handle<StdMatExt<DistanceDither>>);

pub fn spawn_loaded_chunks(
	mut cmds: Commands,
	mut tasks: ResMut<ChunkLoadingTasks>,
	mat: Res<TerrainMaterial>,
	mut loaded_chunks: ResMut<LoadedChunks>,
	frame: Res<Frame>,
) {
	tasks.retain(|index, task| {
		let center = ChunkCenter::from(*index);
		let translation = center.delta_from(&frame.center);
		if let Some((heights, mesh)) = task.check() {
			let heights = Arc::new(heights);
			let id = cmds
				.spawn((
					TerrainObject {
						pbr: MaterialMeshBundle {
							mesh,
							transform: Transform {
								translation: Vec3::new(translation.x, translation.y, 0.0),
								rotation: Quat::from_rotation_x(FRAC_PI_2),
								..default()
							},
							material: mat.0.clone(),
							..default()
						},
						collider: Collider::from(SharedShape(heights.clone())),
						..default()
					},
					*index,
					center,
					Ground { heights },
					NoAutomaticBatching,
				))
				.id();

			loaded_chunks.insert(*index, id);

			// remove from tasks vec
			false
		} else {
			// try again next frame
			true
		}
	})
}

// Pattern ensures that there are always at least 2 chunks beyond the player in any direction.
#[rustfmt::skip]
const NEARBY: [(i32, i32); 37] = [
	                    (-1, -3), (0, -3), ( 1, -3),
	          (-2, -2), (-1, -2), (0, -2), ( 1, -2), ( 2, -2),
	(-3, -1), (-2, -1), (-1, -1), (0, -1), ( 1, -1), ( 2, -1), ( 3, -1),
	(-3,  0), (-2,  0), (-1,  0), (0,  0), ( 1,  0), ( 2,  0), ( 3,  0),
	(-3,  1), (-2,  1), (-1,  1), (0,  1), ( 1,  1), ( 2,  1), ( 3,  1),
	          (-2,  2), (-1,  2), (0,  2), ( 1,  2), ( 2,  2),
	                    (-1,  3), (0,  3), ( 1,  3),
];

pub fn load_nearby_chunks(
	players: Query<&GlobalTransform, ERef<Root>>,
	loaded_chunks: Res<LoadedChunks>,
	mut tasks: ResMut<ChunkLoadingTasks>,
	mut task_offloader: TaskOffloader,
	frame: Res<Frame>,
	assets: Res<AssetServer>,
	noise: Res<PlanetHeightSource>,
) {
	for player in &players {
		let player_pos = frame.planet_coords_of(player.translation().xy());
		for (x, y) in NEARBY {
			let sample = PlanetVec2::new(
				(x as f64 * CHUNK_COLS as f64 * TERRAIN_CELL_SIZE as f64) + player_pos.x,
				(y as f64 * CHUNK_ROWS as f64 * TERRAIN_CELL_SIZE as f64) + player_pos.y,
			);
			let chunk = sample.into();
			if !loaded_chunks.contains_left(&chunk) && !tasks.contains_key(&chunk) {
				info!("Loading {chunk:?}");
				generate_chunk(
					chunk,
					assets.clone(),
					noise.clone(),
					&mut tasks,
					&mut task_offloader,
				);
			}
		}
	}
}

pub fn unload_distant_chunks(
	mut cmds: Commands,
	players: Query<&GlobalTransform, ERef<Root>>,
	chunks: Query<(Entity, &GlobalTransform), With<ChunkCenter>>,
	dist: Res<UnloadDistance>,
	mut loaded_chunks: ResMut<LoadedChunks>,
) {
	if players.is_empty() {
		// All players are despawned, don't unload anything until
		// we can know where they will respawn.
		return;
	}

	// WHY is there no length function on `Vector2`??
	let diameter = (CHUNK_SCALE.x * CHUNK_SCALE.x + CHUNK_SCALE.y * CHUNK_SCALE.y).sqrt();

	// Don't oscillate between loading and unloading
	// Must allow for player to be in the corner of a chunk
	// and still not unload cells in `NEARBY`
	let dist = f32::max(**dist, diameter * 3.6);
	for (id, global) in &chunks {
		let global = global.translation().xy();
		if !players
			.iter()
			.find(|player| (global - player.translation().xy()).length() < dist)
			.is_some()
		{
			cmds.entity(id).despawn();
			if let Some((chunk, _)) = loaded_chunks.remove_by_right(&id) {
				info!("Unloaded {chunk:?}");
			} else {
				warn!("Chunk {id:?} wasn't in the `LoadedChunks` map");
			}
		}
	}
}

#[derive(Resource, Deref, DerefMut, Debug, Reflect)]
pub struct UnloadDistance(f32);
