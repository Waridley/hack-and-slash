#![warn(unused_crate_dependencies)]

use bevy::app::{App, Plugin};
use bevy_svg::SvgPlugin;

pub mod anim;
pub mod draw;
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
		app.add_plugins((ui::UiPlugin, input::InputPlugin, SvgPlugin))
			.register_type::<util::MeshOutline>()
			.add_systems(bevy::prelude::Last, util::MeshOutline::sync);
	}
}
