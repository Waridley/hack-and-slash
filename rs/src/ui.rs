use bevy::{
	prelude::*,
};
use enum_components::EnumComponent;

pub mod pause_menu;

#[derive(Debug, EnumComponent)]
pub enum GameMenu {
	MainMenu,
	PauseMenu,
}
use game_menu::*;


pub struct GameUiPlugin;

impl Plugin for GameUiPlugin {
	fn build(&self, app: &mut App) {
		app.add_system(game_ui_setup);
	}
}

pub fn game_ui_setup(mut cmds: Commands) {
	cmds.spawn((VisibilityBundle::default(),));
}
