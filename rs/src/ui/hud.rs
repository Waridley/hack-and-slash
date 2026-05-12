use bevy::prelude::*;

pub struct HudPlugin;

impl Plugin for HudPlugin {
	fn build(&self, app: &mut App) {
		app.add_systems(Update, setup);
	}
}

pub fn setup(_cmds: Commands) {}
