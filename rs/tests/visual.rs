use bevy::prelude::*;
use sond_has::tests::*;
use bevy::winit::WinitSettings;
use bevy_rapier3d::render::RapierDebugRenderPlugin;

#[bevy_main]
fn main() {
	let mut app = app();
	app
		.add_system(bevy::window::close_on_esc)
			.insert_resource(WinitSettings {
				return_from_run: true,
				..default()
			})
			.add_plugin(RapierDebugRenderPlugin {
				enabled: true,
				..default()
			});
	
	app.run()
}
