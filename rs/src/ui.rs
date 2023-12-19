use crate::util::IntoFnPlugin;
use bevy::prelude::*;
use enum_components::EnumComponent;

#[cfg(feature = "debugging")]
pub mod dbg_ui;
pub mod game_ui;
// pub mod pause_menu;

pub fn plugin(app: &mut App) -> &mut App {
	#[cfg(feature = "debugging")]
	app.add_plugins(dbg_ui::plugin.plugfn());

	app.add_plugins(game_ui::plugin.plugfn())
		.init_resource::<UiHovered>()
	// .add_plugins(pause_menu::plugin.plugfn())
}

#[derive(Debug, EnumComponent)]
pub enum GameMenu {
	MainMenu,
	PauseMenu,
}

/// Keeps track of whether a UI element is hovered over so that clicking
/// does not grab the mouse if so.
#[derive(Resource, Debug, Default, Copy, Clone, Deref, DerefMut, Reflect)]
pub struct UiHovered(bool);
