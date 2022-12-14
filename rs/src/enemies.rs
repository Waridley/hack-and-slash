use crate::util::FnPluginExt;
use bevy::prelude::*;
use enum_components::EnumComponent;

pub mod aa;
pub mod dummy;

pub fn plugin(app: &mut App) -> &mut App {
	app.fn_plugin(dummy::plugin)
}

#[derive(EnumComponent, Reflect, FromReflect)]
pub enum Enemy {
	Dummy,
	AA,
	QuadCopter,
}
