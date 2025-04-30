use std::{f32::consts::*, sync::Arc};

use ::noise::{
	Add, Billow, Fbm, HybridMulti, MultiFractal, Perlin, RidgedMulti, ScaleBias, Seedable, Value,
};
use bevy::{
	prelude::*,
	render::{
		batching::NoAutomaticBatching,
		mesh::{PrimitiveTopology, VertexAttributeValues::Float32x3},
		render_asset::RenderAssetUsages,
	},
	utils::HashMap,
};
use bevy_rapier3d::{
	geometry::shape_views::HeightFieldCellStatus,
	na::Point3,
	parry::shape::{SharedShape, Triangle},
	prelude::*,
};
use rapier3d::{geometry::HeightField, na::DMatrix};
use web_time::{Duration, Instant};

use crate::{
	mats::fog::Matter,
	nav::heightmap::{FnsThatShouldBePub, TriId},
	offloading::{wasm_yield, Offload, OffloadedTask, TaskHandle, TaskOffloader},
	planet::{
		chunks::{
			ChunkCenter, ChunkIndex, LoadedChunks, CHUNK_COLS, CHUNK_ROWS, CHUNK_SCALE,
			TERRAIN_CELL_SIZE,
		},
		frame::Frame,
		PlanetVec2,
	},
	util::Diff,
};
use rng::{
	PlanetSeed,
	terrain::TerrainSeeds,
};

pub mod noise;
pub mod physics;

use noise::{ChooseAndSmooth, Source, SyncWorley};

pub type Noise = ChooseAndSmooth<4>;

pub struct TerrainPlugin;

impl Plugin for TerrainPlugin {
	fn build(&self, app: &mut App) {
		// WHY is there no length function on `Vector2`??
		let diameter = (CHUNK_SCALE.x * CHUNK_SCALE.x + CHUNK_SCALE.y * CHUNK_SCALE.y).sqrt();
		
		app.add_systems(Startup, setup)
			.init_resource::<ChunkLoadingTasks>()
			.insert_resource(UnloadDistance(5.0 * diameter))
			.add_systems(PreUpdate, spawn_loaded_chunks)
			.add_systems(Last, (load_nearby_chunks, unload_distant_chunks));
	}
}
pub fn setup(
	mut cmds: Commands,
	assets: Res<AssetServer>,
	mut chunk_loading_tasks: ResMut<ChunkLoadingTasks>,
	mut task_offloader: TaskOffloader,
) {
	let material = MeshMaterial3d(assets.load("shaders/terrain.mat.ron"));

	cmds.insert_resource(TerrainMaterial(material.0.clone()));
	let seed = rand::random::<PlanetSeed>();
	info!(name: "seed", seed = %seed);

	let noise = PlanetHeightSource::generate((&seed).into());

	// Spawn center first
	generate_chunk(
		ChunkIndex::new(0, 0),
		assets.clone(),
		noise.clone(),
		&mut chunk_loading_tasks,
		&mut task_offloader,
	);

	cmds.insert_resource(seed);
	cmds.insert_resource(noise);

	let mesh = Mesh::from(Cuboid {
		half_size: Vec3::splat(32.0),
	})
	// All of this makes cube meshes use the same compiled shader as heightmap terrain, preventing freezes
	.with_duplicated_vertices()
	.with_computed_flat_normals()
	.with_removed_attribute(Mesh::ATTRIBUTE_UV_0);
	let collider = Collider::cuboid(32.0, 32.0, 32.0);
	let mesh = Mesh3d(assets.add(mesh));

	cmds.insert_resource(TerrainTemplate {
		mesh,
		material,
		collider,
	});
}

pub fn generate_chunk(
	index: ChunkIndex,
	assets: AssetServer,
	noise: PlanetHeightSource,
	chunk_loading_tasks: &mut ChunkLoadingTasks,
	task_offloader: &mut TaskOffloader,
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

			let mut mesh = Mesh::new(
				PrimitiveTopology::TriangleList,
				RenderAssetUsages::RENDER_WORLD,
			);
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
	pub mesh: Mesh3d,
	pub material: MeshMaterial3d<Matter>,
	pub collider: Collider,
}

#[derive(Component, Debug, Clone)]
#[require(Transform, Visibility)]
pub struct Ground {
	pub heights: Arc<HeightField>,
}

impl Ground {
	pub fn new(heights: impl Into<Arc<HeightField>>) -> Self {
		Self {
			heights: heights.into(),
		}
	}

	pub fn tri_at(&self, local_point: Vec2) -> Option<(TriId, Triangle)> {
		// HeightField has to be inverted for chunk indices to follow PlanetVec2
		let rel = Vec2::new(local_point.x, -local_point.y);
		let point = Point3::new(rel.x, 0.0, rel.y);
		let (i, j) = self.heights.cell_at_point(&point)?;
		let (left, right) = self.heights.triangles_at(i, j);

		// ZIG-ZAG
		// a -- c  c
		// | L / / |
		// |  / /  |
		// | / / R |
		// b  a -- b
		//
		// NORMAL
		// a  a -- c
		// | \ \ R |
		// |  \ \  |
		// | L \ \ |
		// b -- c  b
		//
		let tri = match (left, right) {
			(Some(left), Some(right)) => {
				let status = self.heights.cell_status(i, j);
				let (l, r) = if status.contains(HeightFieldCellStatus::ZIGZAG_SUBDIVISION) {
					debug_assert!(
						{
							let p = left.a - right.a;
							p.x * p.x + p.y * p.y + p.z * p.z
						} <= f32::EPSILON
					);
					(left.a.xy(), right.b.xy())
				} else {
					debug_assert!(
						{
							let p = left.c - right.c;
							p.x * p.x + p.y * p.y + p.z * p.z
						} <= f32::EPSILON
					);
					(left.b.xy(), right.c.xy())
				};
				// Checking the squared distances to the opposing points is at least as cheap as any other solution.
				let p = point.xy();
				let dl = l - p.xy();
				let dr = r - p.xy();
				let dl = dl.x * dl.x + dl.y * dl.y;
				let dr = dr.x * dr.x + dr.y * dr.y;
				if dl < dr {
					(self.heights.triangle_id(i, j, true), left)
				} else {
					(self.heights.triangle_id(i, j, false), right)
				}
			}
			(Some(left), None) => (self.heights.triangle_id(i, j, true), left),
			(None, Some(right)) => (self.heights.triangle_id(i, j, false), right),
			(None, None) => return None,
		};
		Some(tri)
	}

	pub fn height_at(&self, local_point: Vec2) -> Option<f32> {
		self.tri_and_height_at(local_point)
			.map(|(_, height)| height)
	}

	pub fn tri_and_height_at(&self, local_point: Vec2) -> Option<(TriId, f32)> {
		let (id, tri) = self.tri_at(local_point)?;
		// HeightField has to be inverted for chunk indices to follow PlanetVec2
		let rel = Vec2::new(local_point.x, -local_point.y);
		let da = TERRAIN_CELL_SIZE - (Vec2::from(tri.a.xz()) - rel).length();
		let db = TERRAIN_CELL_SIZE - (Vec2::from(tri.b.xz()) - rel).length();
		let dc = TERRAIN_CELL_SIZE - (Vec2::from(tri.c.xz()) - rel).length();
		let sum = da + db + dc;
		let height = (da / sum) * tri.a.y + (db / sum) * tri.b.y + (dc / sum) * tri.c.y;
		Some((id, height))
	}
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

	pub fn generate(seeds: TerrainSeeds) -> Self {
		let base = bytemuck::cast::<_, [u32; 2]>(seeds.base());
		let perlin = Perlin::default().set_seed(base[0]);
		let sources = vec![perlin, perlin.set_seed(base[1])];

		// unfortunately this generates sources that are immediately discarded, but `sources` isn't pub
		let mut base = Fbm::<Perlin>::default();
		base.octaves = 2;
		base.frequency = 0.00128;
		base.persistence = 0.45;
		let base = base.set_sources(sources);

		// Add and ScaleBias are not Clone, so we'll use a closure to get multiple copies
		let base = || {
			Add::new(
				ScaleBias::new(
					HybridMulti::<Value>::default()
						.set_frequency(0.00032)
						.set_persistence(0.5),
				)
				.set_scale(2.0),
				base.clone(),
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
		Self::new(noise)
	}
}

#[derive(Resource, Default, Deref, DerefMut)]
pub struct ChunkLoadingTasks(HashMap<ChunkIndex, TaskHandle<(HeightField, Handle<Mesh>)>>);

#[derive(Resource, Clone, Debug, Deref, DerefMut)]
pub struct TerrainMaterial(Handle<Matter>);

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
					Name::new(format!("Chunk({},{})", index.x, index.y)),
					*index,
					center,
					Transform::from_translation(Vec3::new(translation.x, translation.y, 0.0)),
					Ground {
						heights: heights.clone(),
					},
				))
				.with_children(|builder| {
					builder.spawn((
						*index,
						*center,
						Mesh3d(mesh),
						Transform {
							rotation: Quat::from_rotation_x(FRAC_PI_2),
							..default()
						},
						MeshMaterial3d(mat.0.clone()),
						RigidBody::Fixed,
						Collider::from(SharedShape(heights.clone())),
						Restitution::new(0.5),
						Friction::new(0.01),
						Ccd::enabled(),
						Ground {
							heights: heights.clone(),
						},
						NoAutomaticBatching,
					));
				})
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
	                (-1,-3),( 0,-3),( 1,-3),
	        (-2,-2),(-1,-2),( 0,-2),( 1,-2),( 2,-2),
	(-3,-1),(-2,-1),(-1,-1),( 0,-1),( 1,-1),( 2,-1),( 3,-1),
	(-3, 0),(-2, 0),(-1, 0),( 0, 0),( 1, 0),( 2, 0),( 3, 0),
	(-3, 1),(-2, 1),(-1, 1),( 0, 1),( 1, 1),( 2, 1),( 3, 1),
	        (-2, 2),(-1, 2),( 0, 2),( 1, 2),( 2, 2),
	                (-1, 3),( 0, 3),( 1, 3),
];

#[derive(Component, Debug, Copy, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Reflect)]
#[require(Transform)]
#[reflect(Component)]
pub struct NeedsTerrain;

pub fn load_nearby_chunks(
	players: Query<&GlobalTransform, With<NeedsTerrain>>,
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
				info!("Loading chunk {chunk}");
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
	players: Query<&GlobalTransform, With<NeedsTerrain>>,
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
			.any(|player| (global - player.translation().xy()).length() < dist)
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
