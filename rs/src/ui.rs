use crate::util::FnPluginExt;
use bevy::prelude::*;
use enum_components::EnumComponent;

pub mod game_ui;
// pub mod pause_menu;

pub fn plugin(app: &mut App) -> &mut App {
	app.fn_plugin(game_ui::plugin)
	// .fn_plugin(pause_menu::plugin)
}

#[derive(Debug, EnumComponent)]
pub enum GameMenu {
	MainMenu,
	PauseMenu,
}
