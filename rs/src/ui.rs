use crate::player::PlayerId;
use bevy::{
	prelude::*,
	render::view::{Layer, RenderLayers},
};
use engine::ui::GLOBAL_UI_LAYER;
use enum_components::EnumComponent;

use crate::util::IntoFnPlugin;

#[cfg(feature = "debugging")]
pub mod dbg_ui;
pub mod game_ui;
// pub mod pause_menu;

pub fn plugin(app: &mut App) -> &mut App {
	#[cfg(feature = "debugging")]
	app.add_plugins(dbg_ui::plugin.plugfn());

	app.add_plugins(game_ui::plugin.plugfn())
}

#[derive(Debug, EnumComponent)]
pub enum GameMenu {
	MainMenu,
	PauseMenu,
}

pub fn player_ui_layer(player: PlayerId) -> RenderLayers {
	RenderLayers::layer(GLOBAL_UI_LAYER + player.get() as Layer)
}
