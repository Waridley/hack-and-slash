use crate::{
	player::{
		camera::CameraVertSlider,
		ctrl::CtrlVel,
		player_entity::{CamPivot, ReadPlayerEntity},
		BelongsToPlayer, ACCEL, JUMP_VEL, MAX_JUMPS, MAX_SPEED,
	},
	terminal_velocity,
};
use bevy::{math::Vec3Swizzles, prelude::*};
use leafwing_abilities::{cooldown::Cooldown, prelude::*, AbilitiesBundle, Abilitylike};
use leafwing_input_manager::prelude::*;
use std::{
	f32::consts::{PI, TAU},
	time::Duration,
};

pub struct InputPlugin;

impl Plugin for InputPlugin {
	fn build(&self, app: &mut App) {
		app.add_plugin(InputManagerPlugin::<PlayerAction>::default())
			.add_plugin(AbilityPlugin::<PlayerAction>::default())
			.add_system_to_stage(CoreStage::First, setup)
			.add_system(abilities)
			.add_system(jump.before(terminal_velocity));
	}
}

fn setup(
	mut cmds: Commands,
	q: Query<Entity, (With<ActionState<PlayerAction>>, Without<InputStorageTime>)>,
) {
	for id in q.iter() {
		let duration = Duration::from_millis(120);
		let mut timer = Timer::new(duration, TimerMode::Once);
		timer.set_elapsed(duration);
		cmds.entity(id).insert(InputStorageTime(timer));
	}
}

#[derive(Actionlike, Abilitylike, Copy, Clone, Debug, Reflect, FromReflect)]
pub enum PlayerAction {
	Jump,
}

impl PlayerAction {
	pub fn input_map() -> InputMap<Self> {
		use PlayerAction::*;

		InputMap::new([(KeyCode::Back, Jump), (KeyCode::Space, Jump)])
	}

	pub fn cooldown(&self) -> Cooldown {
		use PlayerAction::*;

		let secs = match *self {
			Jump => 2.0,
		};

		Cooldown::from_secs(secs)
	}

	fn cooldowns() -> CooldownState<Self> {
		let mut cooldowns = CooldownState::default();

		for ability in Self::variants() {
			cooldowns.set(ability, ability.cooldown());
		}

		cooldowns
	}

	fn charges() -> ChargeState<Self> {
		use PlayerAction::*;

		ChargeState::default()
			.set(Jump, Charges::simple(MAX_JUMPS as _))
			.build()
	}

	pub fn abilities_bundle() -> AbilitiesBundle<Self> {
		AbilitiesBundle {
			cooldowns: Self::cooldowns(),
			charges: Self::charges(),
		}
	}
}

pub fn abilities(q: Query<AbilityState<PlayerAction>>) {
	for _state in q.iter() {
		// screen_print!("{:#?}", (&state.action_state, &state.charges, &state.cooldowns))
	}
}

#[derive(Component, Resource, Reflect, Default, Debug, Clone)]
struct InputStorageTime(Timer);

fn jump(
	mut q: Query<(
		AbilityState<PlayerAction>,
		&mut CtrlVel,
		&mut InputStorageTime,
	)>,
	t: Res<Time>,
) {
	for (mut state, mut vel, mut storage) in q.iter_mut() {
		storage.0.tick(t.delta());
		let triggered = if storage.0.finished() {
			if state.action_state.just_pressed(PlayerAction::Jump) {
				if state.trigger(PlayerAction::Jump).is_ok() {
					true
				} else {
					storage.0.reset();
					false
				}
			} else {
				false
			}
		} else {
			state.trigger(PlayerAction::Jump).is_ok()
		};

		if triggered {
			vel.linvel.z = JUMP_VEL
		}
	}
}

pub fn look_input(
	mut player_q: Query<(&mut CtrlVel, &BelongsToPlayer)>,
	mut camera_pivot_q: Query<
		(&mut Transform, &mut CameraVertSlider, &BelongsToPlayer),
		ReadPlayerEntity<CamPivot>,
	>,
	kb: Res<Input<KeyCode>>,
	t: Res<Time>,
) {
	for (mut vel, player_id) in player_q.iter_mut() {
		let delta = (TAU / 0.5/* seconds to max angvel */) * t.delta_seconds();

		let mut x_input = false;
		if kb.pressed(KeyCode::Left) {
			x_input = true;
			vel.angvel.z = (-TAU).max(vel.angvel.z - delta);
		}
		if kb.pressed(KeyCode::Right) {
			x_input = true;
			vel.angvel.z = TAU.min(vel.angvel.z + delta);
		}
		if !x_input {
			vel.angvel.z *= 0.8
		}

		let (mut xform, mut slider) = camera_pivot_q
			.iter_mut()
			.find_map(|(xform, slider, owner)| (owner == player_id).then_some((xform, slider)))
			.unwrap();
		if kb.pressed(KeyCode::Up) {
			slider.0 = (slider.0 - delta * 0.1).max(0.0);
		}
		if kb.pressed(KeyCode::Down) {
			slider.0 = (slider.0 + delta * 0.1).min(1.0);
		}

		let angle = (-PI) + ((PI * 0.9) * slider.0);
		xform.rotation = xform.rotation.slerp(Quat::from_rotation_x(angle), delta);
	}
}

pub fn movement_input(mut q: Query<&mut CtrlVel>, kb: Res<Input<KeyCode>>, t: Res<Time>) {
	for mut ctrl_vel in &mut q {
		let (mut x, mut y) = (0.0, 0.0);

		let dt = t.delta_seconds();

		if kb.pressed(KeyCode::W) {
			y += 1.0;
		}
		if kb.pressed(KeyCode::A) {
			x -= 1.0;
		}
		if kb.pressed(KeyCode::S) {
			y -= 1.0;
		}
		if kb.pressed(KeyCode::D) {
			x += 1.0;
		}

		let Vec2 { x, y } = ctrl_vel
			.linvel
			.xy()
			.lerp(Vec2 { x, y }.normalize_or_zero() * MAX_SPEED, ACCEL * dt);

		// Only trigger change detection if actually changed
		if ctrl_vel.linvel.x != x {
			ctrl_vel.linvel.x = x
		}
		if ctrl_vel.linvel.y != y {
			ctrl_vel.linvel.y = y
		}
	}
}
