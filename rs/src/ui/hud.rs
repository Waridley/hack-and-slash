use bevy::prelude::*;

pub fn plugin(app: &mut App) -> &mut App {
	app.add_systems(Update, setup)
}

pub fn setup(_cmds: Commands) {}
