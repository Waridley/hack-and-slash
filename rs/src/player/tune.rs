use crate::{
	player::abilities::{BoosterCharge, WeaponCharge},
	util::Angle,
};
use bevy::prelude::*;
use rapier3d::geometry::Ball;
use serde::{Deserialize, Serialize};

pub fn extract_loaded_params(
	mut cmds: Commands,
	params_assets: Res<Assets<PlayerParams>>,
	mut events: EventReader<AssetEvent<PlayerParams>>,
) {
	for event in events.read() {
		match event {
			AssetEvent::Added { id } | AssetEvent::Modified { id } => {
				cmds.insert_resource(dbg!(params_assets.get(*id).unwrap().clone()));
			}
			_ => {}
		}
	}
}

#[derive(Asset, TypePath, Resource, Serialize, Deserialize, Clone, Debug)]
pub struct PlayerParams {
	pub abil: AbilityParams,
	pub phys: PlayerPhysicsParams,
}

#[derive(Serialize, Deserialize, Clone, Copy, Debug)]
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

#[derive(Serialize, Deserialize, Clone, Copy, Debug)]
pub struct PlayerPhysicsParams {
	pub max_speed: f32,
	pub accel: f32,
	pub gravity: f32,
	pub slide_angle: Angle,
	pub hover_height: f32,
	pub collider: Ball,
}
