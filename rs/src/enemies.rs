use crate::util::IntoFnPlugin;
use bevy::prelude::*;
use enum_components::EnumComponent;

pub mod aa;
pub mod dummy;

pub fn plugin(app: &mut App) -> &mut App {
	app.add_plugins(dummy::plugin.plugfn())
}

#[derive(EnumComponent, Reflect)]
pub enum Enemy {
	Dummy,
	AA,
	QuadCopter,
}
