use bevy::prelude::*;
use sond_has::tests::exit_app;

#[bevy_main]
fn main() {
	App::new()
		.add_plugins(DefaultPlugins)
		.add_system(exit_app)
		.run()
}
