use crate::{
	nav::heightmap::FnsThatShouldBePub,
	offloading::{wasm_yield, Offload, OffloadedTask, TaskHandle, TaskOffloader},
	planet::{
		chunks::{ChunkCenter, ChunkIndex, CHUNK_COLS, CHUNK_ROWS, CHUNK_SCALE},
		terrain::noise::{ChooseAndSmooth, Source, SyncWorley},
		PlanetVec2,
	},
	util::{Factory, Spawnable},
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
};
use bevy_rapier3d::{parry::shape::SharedShape, prelude::*};
use rapier3d::{geometry::HeightField, na::DMatrix};
use std::{f32::consts::*, sync::Arc};
use web_time::{Duration, Instant};

pub mod noise;

pub type Noise = ChooseAndSmooth<4>;

pub fn plugin(app: &mut App) -> &mut App {
	app.add_systems(Startup, setup)
		.add_systems(PostStartup, spawn_boxes)
		.insert_resource(ChunkLoadingTasks::default())
		.add_systems(Update, spawn_loaded_chunks)
}

pub fn setup(
	mut cmds: Commands,
	assets: Res<AssetServer>,
	mut chunk_loading_tasks: ResMut<ChunkLoadingTasks>,
	mut task_offloader: TaskOffloader,
) {
	let material = assets.add(StandardMaterial {
		base_color: Color::rgb(0.1, 0.1, 0.1),
		reflectance: 0.3,
		perceptual_roughness: 0.0,
		..default()
	});

	cmds.insert_resource(TerrainMaterial(material.clone()));

	// Add is not Clone, so we'll use a closer to get multiple copies
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
				.set_persistence(0.45),
		)
	};

	let strength_noise = Fbm::<Perlin>::default()
		.set_frequency(0.00128)
		.set_octaves(2);

	let perlin = Source::new(
		Add::new(
			ScaleBias::new(
				Fbm::<Perlin>::default()
					.set_frequency(0.0256)
					.set_persistence(0.45),
			)
			.set_scale(0.05),
			base(),
		),
		strength_noise.clone(),
		// Constant::new(-1.0),
	);

	let worley = Source::new(
		Add::new(
			ScaleBias::new(SyncWorley::default().set_frequency(0.05)).set_scale(0.1),
			base(),
		),
		strength_noise.clone().set_seed(1),
		// Constant::new(-1.0),
	);

	let billow = Source::new(
		Add::new(
			ScaleBias::new(
				Billow::<Perlin>::default()
					.set_frequency(0.02)
					.set_octaves(2),
			)
			.set_scale(0.04),
			base(),
		),
		strength_noise.clone().set_seed(2),
		// Constant::new(-1.0),
	);

	let ridged = Source::new(
		Add::new(
			ScaleBias::new(RidgedMulti::<Perlin>::default().set_frequency(0.02)).set_scale(0.07),
			base(),
		),
		strength_noise.clone().set_seed(3),
		// Constant::new(-1.0),
	);

	let noise = ChooseAndSmooth::new([perlin, worley, billow, ridged]);

	let noise = Arc::new(noise);

	// Spawn center first
	generate_chunk(
		ChunkIndex::new(0, 0),
		assets.clone(),
		noise.clone(),
		&mut *chunk_loading_tasks,
		&mut task_offloader,
	);
	for i in -3..=3 {
		for j in -3..=3 {
			if i == 0 && j == 0 {
				continue;
			}
			generate_chunk(
				ChunkIndex::new(j, i),
				assets.clone(),
				noise.clone(),
				&mut *chunk_loading_tasks,
				&mut task_offloader,
			);
		}
	}

	let planet_noise = PlanetHeightSource::new(noise);

	cmds.insert_resource(planet_noise);

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
	noise: Arc<Noise>,
	chunk_loading_tasks: &mut ChunkLoadingTasks,
	task_offloader: &mut TaskOffloader<'w, 's>,
) {
	let (columns, rows) = (CHUNK_COLS, CHUNK_ROWS);
	chunk_loading_tasks.push((
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
	));
}

/// Share mesh, material, and collider amongst multiple `TerrainObjects`
#[derive(Resource, Clone)]
pub struct TerrainTemplate {
	pub mesh: Handle<Mesh>,
	pub material: Handle<StandardMaterial>,
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
			pbr: PbrBundle {
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
	pub pbr: PbrBundle,
	pub rigid_body: RigidBody,
	pub collider: Collider,
	pub restitution: Restitution,
	pub friction: Friction,
	pub ccd: Ccd,
}

impl Default for TerrainObject {
	fn default() -> Self {
		Self {
			pbr: PbrBundle::default(),
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

#[derive(Resource)]
pub struct PlanetHeightSource {
	pub noise: Arc<Noise>,
}

impl PlanetHeightSource {
	pub fn new(noise: impl Into<Arc<Noise>>) -> Self {
		Self {
			noise: noise.into(),
		}
	}
	pub fn local(&self, center: PlanetVec2, size: Vec2) -> LocalHeightSource {
		LocalHeightSource {
			center,
			noise: self.noise.clone(),
			size,
		}
	}
}

pub struct LocalHeightSource {
	pub center: PlanetVec2,
	pub noise: Arc<Noise>,
	pub size: Vec2,
}

impl LocalHeightSource {
	pub fn get(&self, j: usize, i: usize) -> f32 {
		let local = Vec2 {
			x: j as f32 - self.size.x * 0.5,
			y: i as f32 - self.size.y * 0.5,
		};
		let point = self.center + local;
		self.noise.get([point.x, point.y]) as f32
	}
}

#[derive(Resource, Default, Deref, DerefMut)]
pub struct ChunkLoadingTasks(Vec<(ChunkIndex, TaskHandle<(HeightField, Handle<Mesh>)>)>);

#[derive(Resource, Clone, Debug, Deref, DerefMut)]
pub struct TerrainMaterial(Handle<StandardMaterial>);

pub fn spawn_loaded_chunks(
	mut cmds: Commands,
	mut tasks: ResMut<ChunkLoadingTasks>,
	mat: Res<TerrainMaterial>,
) {
	tasks.retain_mut(|(index, task)| {
		// TODO: Relative to current frame
		let center = ChunkCenter::from(*index);
		let translation = Vec2::from(center.0);
		if let Some((heights, mesh)) = task.check() {
			let heights = Arc::new(heights);
			cmds.spawn((
				TerrainObject {
					pbr: PbrBundle {
						mesh,
						transform: Transform {
							translation: Vec3::new(translation.x, translation.y, -768.0),
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
			));
			// remove from tasks vec
			false
		} else {
			// try again next frame
			true
		}
	})
}
