use crate::enemies::copter::QuadCopterPlugin;
use bevy::prelude::*;
use enum_components::EnumComponent;
use mine::MinePlugin;

pub mod aa;
pub mod copter;
#[cfg(feature = "debugging")]
pub mod dummy;
pub mod mine;

pub struct EnemiesPlugin;

impl Plugin for EnemiesPlugin {
	fn build(&self, app: &mut App) {
		#[cfg(feature = "debugging")]
		app.add_plugins(dummy::DummyPlugin);
		app.add_plugins((aa::AaPlugin, MinePlugin, QuadCopterPlugin));
	}
}

#[derive(EnumComponent, Reflect)]
pub enum Enemy {
	#[cfg(feature = "debugging")]
	Dummy,
	AA,
	Mine,
	QuadCopter,
}
