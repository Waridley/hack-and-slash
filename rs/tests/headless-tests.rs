use bevy::prelude::*;
use sond_has::tests::Timeout;

#[test]
fn run_app() {
	one_frame().run()
}

#[test]
#[should_panic]
fn time_out() {
	app()
		.insert_resource(Timeout(Timer::from_seconds(3.0, TimerMode::Once)))
		.run()
}

fn app() -> App {
	let mut app = sond_has::tests::app();
	app
		.add_plugins(MinimalPlugins)
		.add_startup_system(setup);
	app
}

fn one_frame() -> App {
	let mut app = sond_has::tests::one_frame();
	app
		.add_plugins(MinimalPlugins)
		.add_startup_system(setup);
	app
}

fn setup(_cmds: Commands) {

}
