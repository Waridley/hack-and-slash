use super::enemy::Dummy;
use crate::{
	anim::{ComponentDelta, StartAnimation},
	planet::{
		chunks::{ChunkFinder, CHUNK_SCALE},
		frame::Frame,
	},
	player::{
		abilities::{Hurt, Sfx},
		player_entity::Root,
	},
	util::{consume_spawn_events, Spawnable},
	Alive,
};
use bevy::{
	ecs::system::{EntityCommands, SystemParamItem},
	prelude::*,
};
use bevy_kira_audio::{Audio, AudioControl};
use bevy_rapier3d::{
	dynamics::LockedAxes,
	math::Vect,
	na::Vector3,
	plugin::RapierContext,
	prelude::{Collider, RigidBody, TransformInterpolation},
};
use enum_components::{ERef, EntityEnumCommands};
use rand::{prelude::IteratorRandom, Rng};
use std::{f32::consts::FRAC_PI_2, time::Duration};

pub fn plugin(app: &mut App) -> &mut App {
	app.add_systems(Startup, setup)
		.add_event::<NewDummy>()
		.insert_resource(DummySpawnTimer(Timer::new(
			Duration::from_secs(15),
			TimerMode::Repeating,
		)))
		.add_systems(Update, (consume_spawn_events::<Dummy>, handle_hits))
		.add_systems(Last, spawn_new_dummies)
}

fn setup(
	mut cmds: Commands,
	mut meshes: ResMut<Assets<Mesh>>,
	mut materials: ResMut<Assets<StandardMaterial>>,
) {
	let mesh = meshes.add(
		shape::Capsule {
			radius: 2.0,
			depth: 4.0,
			..default()
		}
		.into(),
	);
	let material = materials.add(Color::YELLOW.into());
	let collider = Collider::capsule(Vect::NEG_Y * 2.0, Vect::Y * 2.0, 2.0);
	let locked_axes = LockedAxes::ROTATION_LOCKED
		| LockedAxes::TRANSLATION_LOCKED_X
		| LockedAxes::TRANSLATION_LOCKED_Y;
	cmds.insert_resource(DummyTemplate {
		mesh,
		material,
		collider,
		locked_axes,
	});
}

#[derive(Bundle, Default, Clone)]
struct DummyBundle {
	mat_mesh: MaterialMeshBundle<StandardMaterial>,
	body: RigidBody,
	collider: Collider,
	interp: TransformInterpolation,
	locked_axes: LockedAxes,
	alive: Alive,
}

impl Spawnable for Dummy {
	type Params = Res<'static, DummyTemplate>;
	type InstanceData = NewDummy;

	fn spawn<'w, 's, 'a>(
		cmds: &'a mut Commands<'w, 's>,
		params: &mut SystemParamItem<'w, 's, Self::Params>,
		NewDummy { transform }: Self::InstanceData,
	) -> EntityCommands<'w, 's, 'a> {
		let DummyTemplate {
			mesh,
			material,
			collider,
			locked_axes,
		} = params.clone();
		let mut cmds = cmds.spawn(DummyBundle {
			mat_mesh: MaterialMeshBundle {
				mesh,
				material,
				transform,
				..default()
			},
			collider,
			locked_axes,
			..default()
		});
		cmds.set_enum(Dummy);
		cmds
	}
}

#[derive(Resource, Clone)]
pub struct DummyTemplate {
	mesh: Handle<Mesh>,
	material: Handle<StandardMaterial>,
	collider: Collider,
	locked_axes: LockedAxes,
}

#[derive(Event, Default, Debug, Clone)]
pub struct NewDummy {
	transform: Transform,
}

pub fn spawn_new_dummies(
	mut events: EventWriter<NewDummy>,
	mut timer: ResMut<DummySpawnTimer>,
	players: Query<&GlobalTransform, ERef<Root>>,
	frame: Res<Frame>,
	chunks: ChunkFinder,
	t: Res<Time>,
) {
	timer.tick(t.delta());
	if timer.just_finished() {
		let mut rng = rand::thread_rng();
		let player = players
			.iter()
			.choose(&mut rng)
			.map_or(Vec2::ZERO, |global| global.translation().xy());
		let bounds = CHUNK_SCALE.xz() * 1.5;
		let x = rng.gen_range(-bounds.x..=bounds.x);
		let y = rng.gen_range(-bounds.y..=bounds.y);
		let Some(z) = chunks.height_at(frame.planet_coords_of(player - Vec2::new(x, y))) else {
			return;
		};

		events.send(NewDummy {
			transform: Transform {
				translation: Vec3::new(x, y, z + 8.0),
				rotation: Quat::from_rotation_x(FRAC_PI_2),
				..default()
			},
		});
	}
}

#[derive(Resource, Debug, Deref, DerefMut)]
pub struct DummySpawnTimer(Timer);

pub fn handle_hits(
	mut ctx: ResMut<RapierContext>,
	mut cmds: Commands,
	audio: Res<Audio>,
	sfx: Res<Sfx>,
	dummies: Query<(Entity, &GlobalTransform), (ERef<Dummy>, With<Alive>)>,
	mut events: EventReader<Hurt>,
) {
	for event in events.read() {
		if let Ok((id, global)) = dummies.get(event.victim) {
			let Some(toi) = event.toi.details else {
				continue;
			};
			if let Some(body) = ctx
				.entity2body()
				.get(&id)
				.copied()
				.and_then(|body| ctx.bodies.get_mut(body))
			{
				audio.play(sfx.impacts[0].clone());
				body.set_locked_axes(rapier3d::prelude::LockedAxes::empty(), true);
				body.apply_impulse_at_point(
					Vector3::from(global.compute_transform().rotation * toi.normal2) * 2000.0,
					toi.witness1.into(),
					true,
				);
			}
			let mut cmds = cmds.entity(id);
			cmds.insert(LockedAxes::empty());
			cmds.remove::<Alive>();
			let mut timer = Timer::from_seconds(4.0, TimerMode::Once);
			// Just need to animate some component. Don't lock mutable access to transforms for this.
			cmds.start_animation::<Visibility>(move |id, _, t, mut ctrl| {
				timer.tick(t.delta());
				if timer.finished() {
					ctrl.commands().entity(id).despawn();
					ctrl.end();
				}
				ComponentDelta::indefinite(id, |_| ())
			});
		}
	}
}
