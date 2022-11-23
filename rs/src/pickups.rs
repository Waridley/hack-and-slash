use crate::{
	pickups::pickup::PickupItem,
	player::{
		player_entity::{Arm, ReadPlayerEntity},
		RotVel,
	},
};
use bevy::{
	math::Vec3Swizzles,
	prelude::{shape::Icosphere, *},
};
use bevy_kira_audio::{Audio, AudioControl, AudioSource};
use bevy_rapier3d::{
	geometry::Collider,
	plugin::RapierContext,
	prelude::{QueryFilter, RigidBody::KinematicPositionBased, Sensor},
};
use enum_components::{EntityEnumCommands, EnumComponent};
use nanorand::Rng;
use std::{
	f32::consts::TAU,
	sync::atomic::{AtomicI64, Ordering::Relaxed},
	time::Duration,
};

pub static HEALTH: AtomicI64 = AtomicI64::new(0);
pub static SHIELD: AtomicI64 = AtomicI64::new(0);

pub fn plugin(app: &mut App) -> &mut App {
	app.add_startup_system(setup)
		.add_system(collect)
		.add_system(spawn_pickups)
		.add_system(movement)
		.add_system(miss)
}

#[derive(Resource, Default, Debug, Clone, Deref, DerefMut)]
pub struct PopSfx(pub Handle<AudioSource>);

pub fn setup(
	mut cmds: Commands,
	mut meshes: ResMut<Assets<Mesh>>,
	mut materials: ResMut<Assets<StandardMaterial>>,
	asset_server: Res<AssetServer>,
) {
	cmds.insert_resource(SpawnTimer(Timer::new(
		Duration::from_secs(5),
		TimerMode::Repeating,
	)));

	cmds.insert_resource(MissSfx(asset_server.load("sfx/SFX_-_negative_09.ogg")));
	cmds.insert_resource(PopSfx(asset_server.load("sfx/SFX_-_hit_big_02.ogg")));

	let mesh = meshes.add(
		Icosphere {
			radius: 8.0,
			subdivisions: 0,
		}
		.into(),
	);

	let material = materials.add(StandardMaterial {
		base_color: Color::rgba(0.0, 0.0, 0.0, 0.16),
		emissive: Color::rgb(4.0, 0.6, 0.0),
		alpha_mode: AlphaMode::Blend,
		reflectance: 0.36,
		cull_mode: None,
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

#[derive(Resource, Debug, Clone, Deref, DerefMut)]
pub struct SpawnTimer(Timer);

pub fn spawn_pickups(
	mut cmds: Commands,
	handles: Res<PickupAssets>,
	mut rng: Local<PickupRng>,
	mut timer: ResMut<SpawnTimer>,
	t: Res<Time>,
) {
	if timer.tick(t.delta()).finished() {
		let transform = Transform::from_translation(Vec3::new(
			rng.generate::<f32>() * 640.0 - 320.0,
			rng.generate::<f32>() * 640.0 - 320.0,
			-212.0,
		));
		let points = transform.translation.xy().length() * 0.1 + 10.0;

		info!("{:?}", &transform.translation.xy());

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
		.set_enum(if rng.generate::<bool>() {
			Pickup::Health(points)
		} else {
			Pickup::Shield(points)
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
						audio.play(sfx.0.clone()).with_volume(0.3);
						match pickup {
							PickupItem::Health(val) => {
								let val = val.0 as i64;
								let new = HEALTH.fetch_add(val, Relaxed) + val;
								info!("Gained {val} health. Current health: {new}");
							}
							PickupItem::Shield(val) => {
								let val = val.0 as i64;
								let new = SHIELD.fetch_add(val, Relaxed) + val;
								info!("Gained {val} shield. Current shield: {new}");
							}
						}
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

pub fn movement(mut q: Query<&mut Transform, Pickup>, t: Res<Time>) {
	let s = t.elapsed_seconds();
	let dt = t.delta_seconds();
	for mut xform in &mut q {
		xform.translation.z += dt * (4.0 * ((s + xform.translation.x % TAU).sin() + 0.5));
		if xform.translation.z > 128.0 {
			xform.translation.z *= xform.translation.z - 127.0;
		}
		xform.rotation = xform.rotation.slerp(
			Quat::from_euler(
				EulerRot::XYZ,
				(s * (xform.translation.x * 0.001 + 1.0)).sin(),
				(s * (xform.translation.y * 0.001 + 1.0)).sin(),
				(s * (xform.translation.z * 0.001 + 1.0)).sin(),
			),
			dt * 10.0,
		);
	}
}

#[derive(Resource, Debug, Clone, Deref, DerefMut)]
pub struct MissSfx(Handle<AudioSource>);

pub fn miss(
	mut cmds: Commands,
	q: Query<(Entity, &GlobalTransform, Pickup)>,
	miss_sfx: Res<MissSfx>,
	audio: Res<Audio>,
) {
	for (id, xform, pickup) in &q {
		if xform.translation().z > 512.0 {
			audio.play((**miss_sfx).clone()).with_volume(0.2);
			match pickup {
				PickupItem::Health(val) => {
					let val = (val.0 / 2.0) as i64;
					let new = HEALTH.fetch_sub(val, Relaxed) + val;
					info!("Lost {val} health. Remaining: {new}");
				}
				PickupItem::Shield(val) => {
					let val = (val.0 / 2.0) as i64;
					let new = SHIELD.fetch_sub(val, Relaxed) + val;
					info!("Lost {val} shield. Remaining: {new}");
				}
			}
			cmds.entity(id).despawn()
		}
	}
}
