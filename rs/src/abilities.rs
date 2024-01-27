use crate::{
	player::{
		ctrl::CtrlVel,
		input::PlayerAction,
		player_entity::{Arm, Arms},
		tune::{AbilityParams, PlayerParams},
		BelongsToPlayer, RotVel,
	},
	util::Lerp,
};
use bevy::prelude::*;
use bevy_kira_audio::{Audio, AudioControl};
use enum_components::ERef;
use leafwing_input_manager::{
	action_state::ActionState, axislike::DualAxisData, systems::update_action_state,
};
use particles::Spewer;
use serde::{Deserialize, Serialize};
use std::{f32::consts::FRAC_PI_3, time::Duration};

pub struct AbilitiesPlugin;

impl Plugin for AbilitiesPlugin {
	fn build(&self, app: &mut App) {
		app.add_systems(
			Update,
			(trigger_player_abilities, fill_weapons)
				.after(update_action_state::<PlayerAction>)
				.run_if(resource_exists::<PlayerParams>()),
		);
	}
}

pub fn trigger_player_abilities(
	mut cmds: Commands,
	mut q: Query<(
		&ActionState<PlayerAction>,
		&mut BoosterCharge,
		&mut WeaponCharge,
		&mut CtrlVel,
		&BelongsToPlayer,
	)>,
	mut arms_q: Query<&mut RotVel, ERef<Arms>>,
	mut arm_q: Query<(&mut Transform, &mut Spewer), ERef<Arm>>,
	sfx: Res<AoESound>,
	audio: Res<Audio>,
	params: Res<PlayerParams>,
	t: Res<Time>,
) {
	// TODO: Cooldowns
	use PlayerAction::*;
	for (state, mut boost_charge, mut weap_charge, mut vel, owner) in &mut q {
		let AbilityParams {
			jump_cost,
			jump_vel,
			dash_cost,
			dash_vel,
			aoe_cost,
			..
		} = params.abil;
		if state.just_pressed(Jump) && *boost_charge >= jump_cost {
			**boost_charge -= *jump_cost;
			vel.linvel.z = jump_vel
		}

		if state.just_pressed(Dash) && *boost_charge >= dash_cost {
			**boost_charge -= *dash_cost;
			// Use the most-recently-input direction, not current velocity, to dash in the direction the player expects.
			let dir = if let Some(dir) = state
				.clamped_axis_pair(Move)
				.as_ref()
				.and_then(DualAxisData::direction)
			{
				dir.unit_vector()
			} else {
				// Default to forward if no input.
				// Even if already moving in some direction, a lack of movement input indicates
				// the player is expecting the dash button to do the default thing.
				Vec2::new(0.0, 1.0)
			} * dash_vel;
			vel.linvel.x = dir.x;
			vel.linvel.y = dir.y;
		}

		if state.just_pressed(AoE) && *weap_charge >= aoe_cost {
			**weap_charge -= *aoe_cost;
			audio.play(sfx.0.clone()).with_volume(0.5);
			for mut rvel in &mut arms_q {
				**rvel = 36.0;
			}
			for (mut arm, mut spewer) in &mut arm_q {
				// TODO: Filter by player
				arm.translation *= 6.0;
				arm.scale *= 6.0;
				spewer.interval = Duration::from_micros(200);
			}
		}

		// TODO: Use animation instead of limit lerp
		for mut rvel in &mut arms_q {
			**rvel = **rvel + (rvel.quiescent - **rvel) * t.delta_seconds();
		}
		for (i, (mut arm, mut spewer)) in arm_q.iter_mut().enumerate() {
			let resting = Quat::from_rotation_z(i as f32 * FRAC_PI_3 * 2.0) * Vec3::X * 2.0;
			arm.translation = arm.translation.lerp(resting, t.delta_seconds() * 2.0);
			arm.scale = arm.scale.lerp(Vec3::ONE, t.delta_seconds() * 2.0);
			spewer.interval = Duration::from_secs_f32(
				spewer.interval.as_secs_f32().lerp(0.001, t.delta_seconds()),
			);
		}
	}
}

#[derive(
	Component,
	Deref,
	DerefMut,
	Serialize,
	Deserialize,
	Copy,
	Clone,
	Default,
	Debug,
	PartialEq,
	PartialOrd,
)]
pub struct BoosterCharge(pub f32);

#[derive(
	Component,
	Deref,
	DerefMut,
	Serialize,
	Deserialize,
	Copy,
	Clone,
	Default,
	Debug,
	PartialEq,
	PartialOrd,
)]
pub struct WeaponCharge(pub f32);

pub fn fill_weapons(mut q: Query<&mut WeaponCharge>, params: Res<PlayerParams>, t: Res<Time>) {
	for mut charge in &mut q {
		if *charge <= params.abil.weapon_capacity {
			**charge = f32::min(
				*params.abil.weapon_capacity,
				**charge + *params.abil.weapon_fill_per_second * t.delta_seconds(),
			);
		}
	}
}

#[derive(Debug, Resource, Deref, DerefMut)]
pub struct AoESound(pub Handle<bevy_kira_audio::AudioSource>);
