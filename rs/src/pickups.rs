use std::{
	f32::consts::PI,
	sync::atomic::{AtomicI64, Ordering::Relaxed},
	time::Duration,
};

use bevy::prelude::*;
#[cfg(feature = "bevy_kira_audio")]
use bevy_kira_audio::{Audio, AudioControl, AudioSource};
use bevy_rapier3d::{
	geometry::Collider,
	prelude::{RigidBody::KinematicPositionBased, Sensor},
};
use engine::{
	mats::ExtMat,
	planet::{chunks::ChunkCenter, terrain::Ground},
};
use enum_components::{EntityEnumCommands, EnumComponent};
use pickup::WithPickup;
use rand::{rngs::SmallRng, Rng, SeedableRng};

use crate::{mats::BubbleMaterial, pickups::pickup::PickupItem, player::abilities::Hurt};

pub const RADIUS: f32 = 8.0;

pub static HEALTH: AtomicI64 = AtomicI64::new(0);
pub static SHIELD: AtomicI64 = AtomicI64::new(0);

pub fn plugin(app: &mut App) -> &mut App {
	app.add_systems(Startup, setup)
		.insert_resource(PickupRng(SmallRng::from_entropy()))
		.add_systems(Update, (collect, spawn_pickups, movement, miss))
}

#[cfg(feature = "bevy_kira_audio")]
#[derive(Resource, Default, Debug, Clone, Deref, DerefMut)]
pub struct PopSfx(pub Handle<AudioSource>);

pub fn setup(mut cmds: Commands, mut meshes: ResMut<Assets<Mesh>>, asset_server: Res<AssetServer>) {
	const SPAWN_INTERVAL: u64 = 30;
	let mut timer = Timer::new(Duration::from_secs(SPAWN_INTERVAL), TimerMode::Repeating);
	timer.set_elapsed(Duration::from_secs(SPAWN_INTERVAL - 1));
	cmds.insert_resource(SpawnTimer(timer));

	#[cfg(feature = "bevy_kira_audio")]
	cmds.insert_resource(MissSfx(asset_server.load("sfx/SFX_-_negative_09.ogg")));
	#[cfg(feature = "bevy_kira_audio")]
	cmds.insert_resource(PopSfx(asset_server.load("sfx/SFX_-_hit_big_02.ogg")));

	let mesh = meshes.add(Sphere { radius: RADIUS }.mesh().ico(0).unwrap());

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
	material: Handle<ExtMat<BubbleMaterial>>,
}

#[derive(Debug, Clone, Resource, Deref, DerefMut)]
pub struct PickupRng(SmallRng);

#[derive(Resource, Debug, Clone, Deref, DerefMut)]
pub struct SpawnTimer(Timer);

pub fn spawn_pickups(
	mut cmds: Commands,
	handles: Res<PickupAssets>,
	mut rng: ResMut<PickupRng>,
	mut timer: ResMut<SpawnTimer>,
	chunks: Query<(Entity, &Ground), With<ChunkCenter>>,
	t: Res<Time>,
) {
	// Spawns shouldn't be closer together on WASM
	const CHANCE_PER_CHUNK: f32 = crate::planet::chunks::CHUNK_COLS as f32 * 0.002;

	if timer.tick(t.delta()).finished() {
		let bounds = crate::planet::chunks::CHUNK_SCALE.xz();
		for (chunk_entity, ground) in &chunks {
			if rng.gen::<f32>() > CHANCE_PER_CHUNK {
				continue;
			}
			let x = rng.gen_range(-bounds.x..=bounds.x);
			let y = rng.gen_range(-bounds.y..=bounds.y);

			let Some((_, tri)) = ground.tri_at(Vec2::new(x * 0.5, y * 0.5)) else {
				error!(?bounds, "no triangle at [{x}, -{y}]");
				continue;
			};
			// Try to avoid spawning in the middle of a wall.
			// Still won't avoid spawning skewered by spikes.
			// Could either do more checks or just have the bubbles grow from 0 scale before moving up.
			let point = tri.center() - (*tri.normal().unwrap() * RADIUS * 2.0);
			let transform = Transform::from_translation(Vec3::new(point.x, -point.z, point.y));
			let points = rng.gen_range(10.0..=100.0);

			info!("{:?}", &transform.translation);

			let mut cmds = cmds.spawn((
				Mesh3d(handles.mesh.clone()),
				MeshMaterial3d(handles.material.clone()),
				transform,
				Collider::ball(8.0),
				Sensor,
				KinematicPositionBased,
			));
			if rng.gen::<bool>() {
				cmds.set_enum(pickup::Health(points));
			} else {
				cmds.set_enum(pickup::Shield(points));
			};
			let id = cmds.id();
			cmds.commands().entity(chunk_entity).add_child(id);
		}
	}
}

pub fn collect(
	mut cmds: Commands,
	pickups: Query<(Entity, Pickup)>,
	#[cfg(feature = "bevy_kira_audio")] sfx: Res<PopSfx>,
	#[cfg(feature = "bevy_kira_audio")] audio: Res<Audio>,
	mut hits: EventReader<Hurt>,
) {
	for hit in hits.read() {
		if let Ok((id, pickup)) = pickups.get(hit.victim) {
			#[cfg(feature = "bevy_kira_audio")]
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
		}
	}
}

pub fn movement(mut q: Query<&mut Transform, WithPickup>, t: Res<Time>) {
	// TODO: This is getting too complicated, just sample some noise or something
	let s = t.elapsed_secs();
	let dt = t.delta_secs();
	for mut xform in &mut q {
		let rise_speed = s * (0.9 + ((s * 0.001 + xform.translation.y * 1000.0).sin() * 0.2));
		xform.translation.z += dt * (8.0 * ((rise_speed + xform.translation.x).sin() + 0.36));
		// Todo maybe a timer or check the terrain height
		if xform.translation.z > 4096.0 {
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

#[cfg(feature = "bevy_kira_audio")]
#[derive(Resource, Debug, Clone, Deref, DerefMut)]
pub struct MissSfx(pub Handle<AudioSource>);

pub fn miss(
	mut cmds: Commands,
	q: Query<(Entity, &GlobalTransform, Pickup)>,
	#[cfg(feature = "bevy_kira_audio")] _miss_sfx: Res<MissSfx>,
	#[cfg(feature = "bevy_kira_audio")] _audio: Res<Audio>,
) {
	for (id, xform, pickup) in &q {
		// Todo maybe a timer or check the terrain height
		if xform.translation().z > 6192.0 {
			// audio.play((**miss_sfx).clone()).with_volume(0.1);
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
