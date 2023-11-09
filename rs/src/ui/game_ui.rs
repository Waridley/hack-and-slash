use bevy::{
	app::{App, Update},
	prelude::{Commands, VisibilityBundle},
};

pub fn plugin(app: &mut App) -> &mut App {
	app.add_systems(Update, setup)
}

pub fn setup(mut cmds: Commands) {
	cmds.spawn((VisibilityBundle::default(),));
}
