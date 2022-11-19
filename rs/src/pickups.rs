use crate::player::{
	player_entity::{Arm, ReadPlayerEntity},
	RotVel,
};
use bevy::prelude::{
	shape::{Icosphere},
	*,
};
use bevy_rapier3d::{
	geometry::Collider,
	plugin::RapierContext,
	prelude::{QueryFilter, RigidBody::KinematicPositionBased, Sensor},
};
use enum_components::{EntityEnumCommands, EnumComponent};
use nanorand::Rng;
use bevy_kira_audio::{Audio, AudioControl, AudioSource};

pub struct PickupPlugin;

impl Plugin for PickupPlugin {
	fn build(&self, app: &mut App) {
		app.add_startup_system(setup)
			.add_system(collect)
			.add_system(spawn_pickups)
			.add_system(idle);
	}
}

#[derive(Resource, Default, Debug, Clone, Deref, DerefMut)]
pub struct PopSfx(pub Handle<AudioSource>);

pub fn setup(
	mut cmds: Commands,
	mut meshes: ResMut<Assets<Mesh>>,
	mut materials: ResMut<Assets<StandardMaterial>>,
	asset_server: Res<AssetServer>,
) {
	let pop_sfx = asset_server.load("sfx/SFX_-_hit_big_02.ogg");
	cmds.insert_resource(PopSfx(pop_sfx));
	
	let mesh = meshes.add(
		Icosphere {
			radius: 8.0,
			subdivisions: 0,
		}
		.into(),
	);

	let material = materials.add(StandardMaterial {
		base_color: Color::rgba(0.0, 0.0, 0.0, 0.3),
		emissive: Color::rgb(1.0, 0.1, 0.0),
		alpha_mode: AlphaMode::Blend,
		..default()
	});

	cmds.insert_resource(PickupAssets { mesh, material })
}

#[derive(EnumComponent)]
pub enum Pickup {
	Health(f32),
	Shield(f32),
}

#[derive(Debug, Clone, Resource, Reflect, FromReflect)]
pub struct PickupAssets {
	mesh: Handle<Mesh>,
	material: Handle<StandardMaterial>,
}

#[derive(Default, Debug, Clone, Resource, Deref, DerefMut)]
pub struct PickupRng(nanorand::WyRand);

pub fn spawn_pickups(
	mut cmds: Commands,
	pickups: Query<(), Pickup>,
	handles: Res<PickupAssets>,
	mut rng: Local<PickupRng>,
) {
	for i in 0..(16 - pickups.iter().len()) {
		let transform = Transform::from_translation(Vec3::new(
			rng.generate::<f32>() * 512.0 - 256.0,
			rng.generate::<f32>() * 512.0 - 256.0,
			rng.generate::<f32>() * 192.0 - 192.0,
		));

		info!("{:?}", &transform.translation);

		cmds.spawn((
			MaterialMeshBundle {
				mesh: handles.mesh.clone(),
				material: handles.material.clone(),
				transform,
				..default()
			},
			Collider::ball(8.0),
			Sensor,
			KinematicPositionBased,
		))
		.set_enum(if i % 2 == 0 {
			Pickup::Health(rng.generate())
		} else {
			Pickup::Shield(rng.generate())
		});
	}
}

pub fn collect(
	mut cmds: Commands,
	ctx: Res<RapierContext>,
	arms: Query<&RotVel, ReadPlayerEntity<Arm>>,
	pickups: Query<(Entity, Pickup, &GlobalTransform, &Collider)>,
	sfx: Res<PopSfx>,
	audio: Res<Audio>,
) {
	for (id, pickup, xform, col) in &pickups {
		let xform = xform.compute_transform();
		ctx.intersections_with_shape(
			xform.translation,
			xform.rotation,
			col,
			QueryFilter::exclude_fixed().exclude_rigid_body(id),
			|other| {
				if let Ok(rvel) = arms.get(other) {
					if **rvel >= 16.0 {
						audio.play(sfx.0.clone())
							.with_volume(0.3);
						info!("{pickup:?}");
						cmds.entity(id).despawn();
						return true;
					}
				}
				false
			},
		);
	}
}

fn idle(mut q: Query<&mut Transform, Pickup>, mut rng: Local<PickupRng>, t: Res<Time>) {
	for mut xform in &mut q {
		xform.rotation = xform.rotation.slerp(
			Quat::from_euler(
				EulerRot::XYZ,
				(t.elapsed_seconds() * (xform.translation.x * 0.001 + 1.0)).sin(),
				(t.elapsed_seconds() * (xform.translation.y * 0.001 + 1.0)).sin(),
				(t.elapsed_seconds() * (xform.translation.z * 0.001 + 1.0)).sin(),
			),
			t.delta_seconds() * 10.0,
		);
	}
}
