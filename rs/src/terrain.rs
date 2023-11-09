use crate::util::{Factory, Spawnable};
use bevy::ecs::system::{EntityCommands, SystemParamItem};
use bevy::{
	prelude::*,
	render::mesh::{PrimitiveTopology, VertexAttributeValues::Float32x3},
};
use bevy_rapier3d::{parry::shape::SharedShape, prelude::*};
use noise::{Fbm, NoiseFn, Value};
use rapier3d::{
	geometry::HeightField,
	na::{DMatrix, Vector3},
};
use std::{f32::consts::*, sync::Arc};

pub fn plugin(app: &mut App) -> &mut App {
	app.add_systems(Startup, setup)
		.add_systems(PostStartup, spawn_boxes)
}

pub fn setup(
	mut cmds: Commands,
	mut materials: ResMut<Assets<StandardMaterial>>,
	mut meshes: ResMut<Assets<Mesh>>,
) {
	let material = materials.add(StandardMaterial {
		base_color: Color::rgb(0.072, 0.072, 0.072),
		reflectance: 0.1,
		perceptual_roughness: 0.0,
		..default()
	});

	let noise = Fbm::<Value>::default();

	let r = 48;
	let d = r * 2;
	let (columns, rows) = (d, d);

	let heights = (0..(rows * columns))
		.map(|i: usize| {
			let col = i % d;
			let row = i / d;

			let r = r as f32 - 2.0;
			let x = (col as f32 - 1.0) - r;
			let y = (row as f32 - 1.0) - r;

			let bowl = ((r * r) - (x * x) - (y * y)).sqrt() / r;

			let bowl = if bowl.is_finite() { bowl } else { 0.0 };

			noise.get([row as f64 * 0.2, (i % rows) as f64 * 0.2]) as f32 * 0.3 - (bowl * 20.0)
		}) // scale to -1.0..=1.0
		.collect();

	let heights = HeightField::new(
		DMatrix::from_vec(rows, columns, heights),
		Vector3::new(1024.0, 24.0, 1024.0),
	);
	let tris = heights.triangles();

	let mut mesh = Mesh::new(PrimitiveTopology::TriangleList);
	let vertices = tris
		.flat_map(|pos| {
			[
				[pos.a.x, pos.a.y, pos.a.z],
				[pos.b.x, pos.b.y, pos.b.z],
				[pos.c.x, pos.c.y, pos.c.z],
			]
		})
		.collect();
	mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, Float32x3(vertices));
	mesh.compute_flat_normals();
	let mesh = meshes.add(mesh);

	let heights = Arc::new(heights);
	cmds.spawn((
		RigidBody::Fixed,
		Collider::from(SharedShape(heights.clone())),
		PbrBundle {
			mesh,
			transform: Transform {
				translation: Vec3::new(0.0, 0.0, 256.0),
				rotation: Quat::from_rotation_x(FRAC_PI_2),
				..default()
			},
			material: material.clone(),
			..default()
		},
		Ground { heights },
	));

	let mesh = Mesh::from(shape::Cube { size: 64.0 });
	let collider = Collider::cuboid(32.0, 32.0, 32.0);
	let mesh = meshes.add(mesh);

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
			mat_mesh_bundle: PbrBundle {
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
	pub mat_mesh_bundle: PbrBundle,
	pub rigid_body: RigidBody,
	pub collider: Collider,
	pub restitution: Restitution,
	pub friction: Friction,
	pub ccd: Ccd,
}

impl Default for TerrainObject {
	fn default() -> Self {
		Self {
			mat_mesh_bundle: PbrBundle::default(),
			rigid_body: RigidBody::Fixed,
			collider: Collider::default(),
			restitution: Restitution::new(0.5),
			friction: Friction::new(0.01),
			ccd: Ccd::disabled(),
		}
	}
}

#[derive(Component, Debug, Clone)]
pub struct Ground {
	pub heights: Arc<HeightField>,
}
