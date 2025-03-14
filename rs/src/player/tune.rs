use bevy::prelude::*;
use bevy_rapier3d::{parry::shape::SharedShape, prelude::Collider};
use rapier3d::prelude::Ball;
use serde::{Deserialize, Serialize};

use crate::{
	player::{
		abilities::{BoosterCharge, WeaponCharge},
		AntigravCollider,
	},
	util::Angle,
};

/// Global game tuning parameters that will be effectively constant in releases.
/// Implemented as an Asset for easy reloading during development.
#[derive(Default, Resource, Serialize, Deserialize, Clone, Debug, Reflect)]
#[reflect(Resource, Default)]
pub struct PlayerParams {
	pub abil: AbilityParams,
	pub phys: PlayerPhysicsParams,
}

#[derive(Default, Serialize, Deserialize, Clone, Copy, Debug, Reflect)]
pub struct AbilityParams {
	pub grounded_booster_charge: BoosterCharge,
	pub weapon_capacity: WeaponCharge,
	pub weapon_fill_per_second: WeaponCharge,
	pub jump_cost: BoosterCharge,
	pub jump_vel: f32,
	pub dash_cost: BoosterCharge,
	pub dash_vel: f32,
	pub aoe_cost: WeaponCharge,
}

#[derive(Default, Serialize, Deserialize, Clone, Copy, Debug, Reflect)]
pub struct PlayerPhysicsParams {
	pub max_speed: f32,
	pub accel: f32,
	pub gravity: f32,
	pub slide_angle: Angle,
	pub hover_height: f32,
	pub collider: PlayerCollider,
	pub antigrav_collider: PlayerCollider,
	pub antigrav_stiffness: f32,
	pub antigrav_spring_damping: f32,
}

/// Reflect Proxy for Rapier collider.
#[derive(Serialize, Deserialize, Clone, Copy, Debug, Reflect)]
pub struct PlayerCollider {
	pub radius: f32,
}

impl From<PlayerCollider> for Collider {
	fn from(value: PlayerCollider) -> Self {
		Self::from(SharedShape::new(Ball {
			radius: value.radius,
		}))
	}
}

// Unfortunately `Default` is needed for reflection, even though the
// actual default will be defined by `globals.scn.ron`
impl Default for PlayerCollider {
	fn default() -> Self {
		Self { radius: 1.0 }
	}
}
