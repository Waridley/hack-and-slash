#![warn(unused_crate_dependencies)]

use crate::util::{
	debug_component_names, error_component_names, DEBUG_COMPONENTS, ERROR_COMPONENTS,
};
use bevy::app::{App, Plugin};
#[allow(unused_imports, clippy::single_component_path_imports)]
#[cfg(all(debug_assertions, not(target_arch = "wasm32")))]
use bevy_dylib;
use bevy_svg::SvgPlugin;

pub mod anim;
pub mod input;
pub mod mats;
pub mod nav;
pub mod offloading;
pub mod planet;
pub mod settings;
#[cfg(feature = "testing")]
pub mod testing;
pub mod ui;
pub mod util;

#[derive(Debug)]
pub struct EnginePlugin;

impl Plugin for EnginePlugin {
	fn build(&self, app: &mut App) {
		app.add_plugins((ui::UiPlugin, input::InputPlugin, SvgPlugin));
		DEBUG_COMPONENTS
			.set(app.world.register_system(debug_component_names))
			.expect("`DEBUG_COMPONENTS` shouldn't already be set");
		ERROR_COMPONENTS
			.set(app.world.register_system(error_component_names))
			.expect("`ERROR_COMPONENTS` shouldn't already be set");
	}
}
