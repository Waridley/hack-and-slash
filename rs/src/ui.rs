use bevy::prelude::*;
use enum_components::EnumComponent;

use crate::util::IntoFnPlugin;

#[cfg(feature = "debugging")]
pub mod dbg;
pub mod hud;
pub mod menus;

pub fn plugin(app: &mut App) -> &mut App {
	#[cfg(feature = "debugging")]
	app.add_plugins(dbg::plugin.plugfn());

	app.add_plugins(hud::plugin.plugfn())
}

#[derive(Debug, EnumComponent)]
pub enum GameMenu {
	MainMenu,
	PauseMenu,
}
