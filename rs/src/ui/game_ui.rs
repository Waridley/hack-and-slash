use bevy::{
	app::App,
	prelude::{Commands, VisibilityBundle},
};

pub fn plugin(app: &mut App) -> &mut App {
	app.add_system(setup)
}

pub fn setup(mut cmds: Commands) {
	cmds.spawn((VisibilityBundle::default(),));
}
