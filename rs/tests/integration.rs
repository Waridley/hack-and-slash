use bevy::prelude::*;
use sond_has::tests::*;

#[bevy_main]
fn main() {
	let mut app = app();

	#[cfg(feature = "vis_test")]
	app.add_system(bevy::window::close_on_esc)
		.insert_resource(bevy::winit::WinitSettings {
			return_from_run: true,
			..default()
		})
		.add_plugin(bevy_rapier3d::render::RapierDebugRenderPlugin {
			enabled: true,
			..default()
		});

	app.run()
}
