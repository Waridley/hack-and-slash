use crate::{enemies::copter::QuadCopterPlugin, util::IntoFnPlugin};
use bevy::prelude::*;
use enum_components::EnumComponent;
use mine::MinePlugin;

pub mod aa;
pub mod copter;
#[cfg(feature = "debugging")]
pub mod dummy;
pub mod mine;

pub fn plugin(app: &mut App) -> &mut App {
	#[cfg(feature = "debugging")]
	app.add_plugins(dummy::plugin.plugfn());
	app.add_plugins((aa::plugin.plugfn(), MinePlugin, QuadCopterPlugin));
	app
}

#[derive(EnumComponent, Reflect)]
pub enum Enemy {
	#[cfg(feature = "debugging")]
	Dummy,
	AA,
	Mine,
	QuadCopter,
}
