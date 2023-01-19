use super::enemy::Dummy;
use crate::util::{consume_spawn_events, Spawnable};
use bevy::ecs::system::{EntityCommands, SystemParamItem};
use bevy::prelude::*;
use enum_components::EntityEnumCommands;

pub fn plugin(app: &mut App) -> &mut App {
	app.add_startup_system(setup)
		.add_event::<NewDummy>()
		.add_system(consume_spawn_events::<Dummy>)
}

fn setup(
	mut cmds: Commands,
	mut meshes: ResMut<Assets<Mesh>>,
	mut materials: ResMut<Assets<StandardMaterial>>,
) {
	let mesh = meshes.add(shape::Cube::new(4.0).into());
	let material = materials.add(Color::YELLOW.into());
	cmds.insert_resource(DummyTemplate { mesh, material });
}

#[derive(Bundle, Default, Clone)]
struct DummyBundle {
	mat_mesh: MaterialMeshBundle<StandardMaterial>,
}

impl Spawnable for Dummy {
	type Params = Res<'static, DummyTemplate>;
	type InstanceData = NewDummy;

	fn spawn<'w, 's, 'a>(
		cmds: &'a mut Commands<'w, 's>,
		params: &mut SystemParamItem<'w, 's, Self::Params>,
		NewDummy { transform }: Self::InstanceData,
	) -> EntityCommands<'w, 's, 'a> {
		let DummyTemplate { mesh, material } = params.clone();
		let mut cmds = cmds.spawn(DummyBundle {
			mat_mesh: MaterialMeshBundle {
				mesh,
				material,
				transform,
				..default()
			},
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
}

#[derive(Default, Debug, Clone)]
pub struct NewDummy {
	transform: Transform,
}
