use super::enemy::Dummy;
use crate::util::{consume_spawn_events, Spawnable};
use bevy::{
	ecs::system::{EntityCommands, SystemParamItem},
	prelude::*,
};
use bevy_rapier3d::{
	math::Vect,
	prelude::{Collider, RigidBody},
};
use enum_components::EntityEnumCommands;
use std::time::Duration;

pub fn plugin(app: &mut App) -> &mut App {
	app.add_systems(Startup, setup)
		.add_event::<NewDummy>()
		.insert_resource(DummySpawnTimer(Timer::new(
			Duration::from_secs(15),
			TimerMode::Repeating,
		)))
		.add_systems(Update, consume_spawn_events::<Dummy>)
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
	cmds.insert_resource(DummyTemplate {
		mesh,
		material,
		collider,
	});
}

#[derive(Bundle, Default, Clone)]
struct DummyBundle {
	mat_mesh: MaterialMeshBundle<StandardMaterial>,
	body: RigidBody,
	collider: Collider,
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
		} = params.clone();
		let mut cmds = cmds.spawn(DummyBundle {
			mat_mesh: MaterialMeshBundle {
				mesh,
				material,
				transform,
				..default()
			},
			collider,
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
}

#[derive(Event, Default, Debug, Clone)]
pub struct NewDummy {
	transform: Transform,
}

pub fn spawn_new_dummies(
	mut events: EventWriter<NewDummy>,
	mut timer: ResMut<DummySpawnTimer>,
	t: Res<Time>,
) {
	timer.tick(t.delta());
	if timer.just_finished() {
		events.send(NewDummy {
			transform: Transform::default(),
		});
	}
}

#[derive(Resource, Debug, Deref, DerefMut)]
pub struct DummySpawnTimer(Timer);
