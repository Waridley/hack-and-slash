use bevy::{app::AppExit, prelude::*};
use bevy_rapier3d::plugin::RapierPhysicsPlugin;

pub fn app() -> App {
	let mut app = App::new();
	app
		.add_plugin(RapierPhysicsPlugin::<()>::default())
		.insert_resource(Timeout(Timer::from_seconds(60.0, TimerMode::Once)))
		.add_system(timeout);
	app
}

pub fn one_frame() -> App {
	let mut app = app();
	app.add_system_to_stage(CoreStage::Last, exit_app);
	app
}

#[derive(Resource, Deref, DerefMut)]
pub struct Timeout(pub Timer);

pub fn timeout(mut timer: ResMut<Timeout>, t: Res<Time>) {
	if timer.tick(t.delta()).finished() {
		panic!("Timed out")
	}
}

pub fn exit_app(mut events: EventWriter<AppExit>) {
	events.send(AppExit)
}
