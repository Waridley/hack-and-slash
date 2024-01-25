use crate::{
	anim::*,
	player::{input::PlayerAction, BelongsToPlayer},
};
use bevy::prelude::*;
use leafwing_input_manager::action_state::ActionState;
use leafwing_input_manager::axislike::DualAxisData;
use leafwing_input_manager::systems::update_action_state;
use crate::player::{DASH_COST, DASH_VEL, JUMP_COST, JUMP_VEL};
use crate::player::ctrl::CtrlVel;

pub struct AbilitiesPlugin;

impl Plugin for AbilitiesPlugin {
	fn build(&self, app: &mut App) {
		app.add_systems(Update, trigger_player_abilities.after(update_action_state::<PlayerAction>));
	}
}

pub fn trigger_player_abilities(
	mut cmds: Commands,
	mut q: Query<(
		&ActionState<PlayerAction>,
		&mut BoosterCharge,
		&mut CtrlVel,
		&BelongsToPlayer,
	)>,
) {
	use PlayerAction::*;
	for (state, mut charge, mut vel, owner) in &mut q {
		if state.just_pressed(Jump) && **charge >= JUMP_COST {
			**charge = f32::max(0.0, **charge - JUMP_COST);
			vel.linvel.z = JUMP_VEL
		}
		
		if state.just_pressed(Dash) && **charge >= DASH_COST {
			**charge = f32::max(0.0, **charge - DASH_COST);
			// Use the just-input direction, not current velocity, to dash in the direction the player expects
			let dir = if let Some(dir) = state.clamped_axis_pair(Move).as_ref().and_then(DualAxisData::direction) {
				dir.unit_vector()
			} else {
				// Forward
				Vec2::new(0.0, 1.0)
			} * DASH_VEL;
			vel.linvel.x = dir.x;
			vel.linvel.y = dir.y;
		}
	}
}

#[derive(Component, Deref, DerefMut, Debug, PartialEq, PartialOrd)]
pub struct BoosterCharge(pub f32);

impl Default for BoosterCharge {
	fn default() -> Self {
		Self(JUMP_VEL * 2.0)
	}
}