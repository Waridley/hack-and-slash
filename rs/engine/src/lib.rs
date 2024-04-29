#![warn(unused_crate_dependencies)]

use bevy::app::{App, Plugin};
#[allow(unused_imports, clippy::single_component_path_imports)]
#[cfg(all(feature = "bevy_dylib", not(target_arch = "wasm32")))]
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
	}
}
