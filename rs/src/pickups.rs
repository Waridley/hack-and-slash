use crate::{
	mats::BubbleMaterial,
	pickups::pickup::PickupItem,
	player::{
		player_entity::{Arm, Arms},
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
use enum_components::{ERef, EntityEnumCommands, EnumComponent};
use nanorand::Rng;
use std::{
	f32::consts::PI,
	sync::atomic::{AtomicI64, Ordering::Relaxed},
	time::Duration,
};

pub static HEALTH: AtomicI64 = AtomicI64::new(0);
pub static SHIELD: AtomicI64 = AtomicI64::new(0);

pub fn plugin(app: &mut App) -> &mut App {
	app.add_systems(Startup, setup)
		.add_systems(Update, (collect, spawn_pickups, movement, miss))
}

#[derive(Resource, Default, Debug, Clone, Deref, DerefMut)]
pub struct PopSfx(pub Handle<AudioSource>);

pub fn setup(mut cmds: Commands, mut meshes: ResMut<Assets<Mesh>>, asset_server: Res<AssetServer>) {
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
		.try_into()
		.expect("create icosphere mesh"),
	);

	let material = asset_server.load("pickups/pickup_material.mat.ron");

	cmds.insert_resource(PickupAssets { mesh, material })
}

#[derive(EnumComponent)]
#[component(derive(Debug, Copy, Clone, PartialEq))]
pub enum Pickup {
	Health(f32),
	Shield(f32),
}

#[derive(Debug, Clone, Resource, Reflect)]
pub struct PickupAssets {
	mesh: Handle<Mesh>,
	material: Handle<BubbleMaterial>,
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
			rng.generate::<f32>() * 1280.0 - 640.0,
			rng.generate::<f32>() * 1280.0 - 640.0,
			-848.0,
		));
		let points = transform.translation.xy().length() * 0.1 + 10.0;

		info!("{:?}", &transform.translation.xy());

		let mut cmds = cmds.spawn((
			MaterialMeshBundle {
				mesh: handles.mesh.clone(),
				material: handles.material.clone(),
				transform,
				..default()
			},
			Collider::ball(8.0),
			Sensor,
			KinematicPositionBased,
		));
		if rng.generate::<bool>() {
			cmds.set_enum(pickup::Health(points));
		} else {
			cmds.set_enum(pickup::Shield(points));
		};
	}
}

pub fn collect(
	mut cmds: Commands,
	ctx: Res<RapierContext>,
	arms_q: Query<&RotVel, ERef<Arms>>,
	arm_q: Query<(), ERef<Arm>>,
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
				if let Ok(()) = arm_q.get(other) {
					let rvel = arms_q.single();
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
	// TODO: This is getting too complicated, just sample some noise or something
	let s = t.elapsed_seconds();
	let dt = t.delta_seconds();
	for mut xform in &mut q {
		let rise_speed = s * (0.9 + ((s * 0.001 + xform.translation.y * 1000.0).sin() * 0.2));
		xform.translation.z += dt * (4.0 * ((rise_speed + xform.translation.x).sin() + 0.36));
		if xform.translation.z > 128.0 {
			xform.translation.z *= 1.003;
		}
		xform.rotation = xform.rotation.slerp(
			Quat::from_euler(
				EulerRot::XYZ,
				((s * 0.17) + xform.translation.x).sin() * PI,
				((s * 0.23) + xform.translation.y).sin() * PI,
				((s * 0.41) + (xform.translation.z * 0.01)).sin() * PI,
			),
			dt,
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
			audio.play((**miss_sfx).clone()).with_volume(0.1);
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
