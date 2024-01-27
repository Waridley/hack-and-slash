use crate::{
	anim::{
		BlendableAnimation, BlendableTrack, IsFinished, LerpSlerpTrack, LerpTrack, MutAnimation,
		SmoothStepTrack, StartAnimation, Track,
	},
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
use std::time::Duration;

pub struct AbilitiesPlugin;

impl Plugin for AbilitiesPlugin {
	fn build(&self, app: &mut App) {
		app.add_systems(
			Update,
			(
				(trigger_player_abilities, fill_weapons)
					.after(update_action_state::<PlayerAction>)
					.run_if(resource_exists::<PlayerParams>()),
				BlendableAnimation::<RotVel, f32>::animate,
				MutAnimation::<Spewer>::animate,
			),
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
	arms_q: Query<(Entity, &RotVel, &BelongsToPlayer), ERef<Arms>>,
	arm_q: Query<(Entity, &Transform, &Spewer, &BelongsToPlayer), ERef<Arm>>,
	sfx: Res<AoESound>,
	audio: Res<Audio>,
	params: Res<PlayerParams>,
) {
	// TODO: Cooldowns
	use PlayerAction::*;
	for (state, mut boost_charge, mut weap_charge, mut vel, player) in &mut q {
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
			for (id, rvel, owner) in &arms_q {
				if owner != player {
					continue;
				}
				cmds.entity(id).start_blendable_animation::<RotVel, f32>(
					LerpTrack::new(
						rvel.quiescent(),
						rvel.with_current(36.0),
						Duration::from_millis(48),
					)
					.chain_blendable(SmoothStepTrack::new(
						rvel.with_current(36.0),
						rvel.quiescent(),
						Duration::from_secs_f32(1.0),
					)),
				);
			}
			for (id, arm, spewer, owner) in &arm_q {
				if owner != player {
					continue;
				}
				let outer = Transform {
					translation: arm.translation * 6.0,
					scale: arm.scale * 6.0,
					..*arm
				};
				cmds.entity(id).start_blendable_animation(
					LerpSlerpTrack::new(*arm, outer, Duration::from_millis(48)).chain_blendable(
						SmoothStepTrack::new(outer, *arm, Duration::from_secs_f32(1.0)),
					),
				);

				let mut elapsed = 0.0;
				let end = spewer.interval.as_secs_f32();
				cmds.entity(id)
					.start_mut_animation(Track::new_mut::<Spewer>(move |mut spewer, t| {
						elapsed += t.delta_seconds();
						let t = f32::min(1.0, elapsed / 1.048);
						spewer.interval = Duration::from_secs_f32(0.00001.lerp(end, t));
						IsFinished::from(t == 1.0)
					}));
			}
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
