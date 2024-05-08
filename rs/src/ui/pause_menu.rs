use bevy::prelude::*;

pub struct PauseMenuPlugin;

impl Plugin for PauseMenuPlugin {
	fn build(&self, app: &mut App) {
		app.add_systems(Startup, setup);
	}
}

pub fn setup(mut cmds: Commands) {}
