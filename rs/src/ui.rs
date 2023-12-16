use crate::util::IntoFnPlugin;
use bevy::prelude::*;
use enum_components::EnumComponent;

pub mod game_ui;
// pub mod pause_menu;

pub fn plugin(app: &mut App) -> &mut App {
	app.add_plugins(game_ui::plugin.plugfn())
	// .add_plugins(pause_menu::plugin.plugfn())
}

#[derive(Debug, EnumComponent)]
pub enum GameMenu {
	MainMenu,
	PauseMenu,
}
