use bevy::{
	prelude::*,
	render::mesh::{PrimitiveTopology, VertexAttributeValues::Float32x3},
};
use bevy_rapier3d::{parry::shape::SharedShape, prelude::*};
use rapier3d::{
	geometry::HeightField,
	na::{DMatrix, Vector3},
};
use std::{f32::consts::*, sync::Arc};
use bevy::ecs::system::EntityCommands;

pub fn plugin(app: &mut App) -> &mut App {
	app
		.add_startup_system(setup)
}

pub fn setup(
	mut cmds: Commands,
	mut materials: ResMut<Assets<StandardMaterial>>,
	mut meshes: ResMut<Assets<Mesh>>,
) {
	let material = materials.add(StandardMaterial {
		base_color: Color::rgb(0.024, 0.0, 0.064),
		reflectance: 0.032,
		..default()
	});
	
	use noises_and_patterns::{noise::Noise, FP};
	let noise = noises_and_patterns::noise::value::Value::new();
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
			
			// sphere
			let bowl = ((r * r) - (x * x) - (y * y)).sqrt() / r;
			
			let bowl = if bowl.is_finite() { bowl } else { 0.0 };
			
			noise.fbm_2d((row as FP * 0.3, (i % rows) as FP * 0.3), 3) - (bowl * 20.0)
		}) // scale to -1.0..=1.0
		.collect();
	
	let heightfield = HeightField::new(
		DMatrix::from_vec(rows, columns, heights),
		Vector3::new(1024.0, 24.0, 1024.0),
	);
	let tris = heightfield.triangles();
	
	let mut mesh = Mesh::new(PrimitiveTopology::TriangleList);
	let vertices = tris
		.flat_map(|pos| {
			[
				[pos.a.x, pos.a.y, pos.a.z],
				[pos.b.x, pos.b.y, pos.b.z],
				[pos.c.x, pos.c.y, pos.c.z],
			]
		})
		.collect::<Vec<_>>();
	mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, Float32x3(vertices));
	mesh.compute_flat_normals();
	let mesh = meshes.add(mesh);
	
	cmds.spawn((
		RigidBody::Fixed,
		Collider::from(SharedShape(Arc::new(heightfield))),
		MaterialMeshBundle::<StandardMaterial> {
			mesh,
			transform: Transform {
				translation: Vec3::new(0.0, 0.0, 256.0),
				rotation: Quat::from_rotation_x(FRAC_PI_2),
				..default()
			},
			material: material.clone(),
			..default()
		},
	));
	
	let mesh = Mesh::from(shape::Cube { size: 64.0 });
	let collider = Collider::cuboid(32.0, 32.0, 32.0);
	let mesh = meshes.add(mesh);
	
	let mut factory = TerrainFactory {
		cmds: &mut cmds,
		mesh,
		material,
		collider,
	};
	
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
pub struct TerrainFactory<'c, 'w: 'c, 's: 'c> {
	pub cmds: &'c mut Commands<'w, 's>,
	pub mesh: Handle<Mesh>,
	pub material: Handle<StandardMaterial>,
	pub collider: Collider,
}

impl<'c, 'w: 'c, 's: 'c> TerrainFactory<'c, 'w, 's> {
	fn spawn<'a>(&'a mut self, transform: Transform) -> EntityCommands<'w, 's, 'a> {
		self.cmds.spawn(TerrainObject {
			mat_mesh_bundle: MaterialMeshBundle {
				mesh: self.mesh.clone(),
				material: self.material.clone(),
				transform,
				..default()
			},
			collider: self.collider.clone(),
			..default()
		})
	}
}

#[derive(Bundle)]
pub struct TerrainObject {
	pub mat_mesh_bundle: MaterialMeshBundle<StandardMaterial>,
	pub rigid_body: RigidBody,
	pub collider: Collider,
	pub restitution: Restitution,
	pub friction: Friction,
	pub ccd: Ccd,
}

impl Default for TerrainObject {
	fn default() -> Self {
		Self {
			mat_mesh_bundle: MaterialMeshBundle::default(),
			rigid_body: RigidBody::Fixed,
			collider: Collider::default(),
			restitution: Restitution::new(0.5),
			friction: Friction::new(0.01),
			ccd: Ccd::disabled(),
		}
	}
}