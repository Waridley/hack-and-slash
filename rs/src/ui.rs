use bevy::prelude::*;
use enum_components::EnumComponent;
use pause_menu::PauseMenuPlugin;

use crate::util::IntoFnPlugin;

#[cfg(feature = "debugging")]
pub mod dbg_ui;
pub mod hud;
pub mod pause_menu;

pub fn plugin(app: &mut App) -> &mut App {
	#[cfg(feature = "debugging")]
	app.add_plugins(dbg_ui::plugin.plugfn());

	app.add_plugins((hud::plugin.plugfn(), PauseMenuPlugin))
}
