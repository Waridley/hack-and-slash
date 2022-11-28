use bevy::app::App;
use bevy::MinimalPlugins;
use sond_has::tests::exit_app;

#[test]
fn run_app() {
	app()
		.add_system(exit_app)
		.run()
}

fn app() -> App {
	let mut app = App::new();
	app.add_plugins(MinimalPlugins);
	app
}
