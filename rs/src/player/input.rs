use std::{
	f32::consts::{FRAC_PI_2, TAU},
	time::Duration,
};
use std::fmt::Formatter;
use bevy::{
	input::mouse::MouseMotion,
	prelude::{
		GamepadAxis::{LeftStickX, LeftStickY, RightStickX, RightStickY},
		*,
	},
	window::CursorGrabMode,
};
use enum_components::WithVariant;
use leafwing_input_manager::prelude::*;
use leafwing_input_manager::prelude::updating::CentralInputStore;
use serde::{Deserialize, Serialize};
use engine::input::ActionExt;
use engine::ui::UiHovered;

use crate::{player::{
	camera::CameraVertSlider, ctrl::CtrlVel, player_entity::CamPivot, prefs::LookSensitivity,
	tune::PlayerParams, BelongsToPlayer,
}, terminal_velocity, TerminalVelocity};

#[derive(SystemSet, Copy, Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InputSystems;

pub fn plugin(app: &mut App) -> &mut App {
	app.add_plugins((InputManagerPlugin::<PlayerAction>::default(),))
		.add_systems(First, setup)
		.add_systems(
			Update,
			((
				look_input.ambiguous_with(movement_input),
				movement_input.run_if(resource_exists::<PlayerParams>),
			)
				.before(terminal_velocity)
				.in_set(InputSystems),),
		)
}

fn setup(
	mut cmds: Commands,
	q: Query<Entity, (With<ActionState<PlayerAction>>, Without<InputStorageTimer>)>,
) {
	for id in q.iter() {
		let duration = Duration::from_millis(64);
		let mut timer = Timer::new(duration, TimerMode::Once);
		timer.set_elapsed(duration);
		cmds.entity(id).insert(InputStorageTimer(timer));
	}
}

#[derive(
	Actionlike,
	Copy,
	Clone,
	Debug,
	PartialEq,
	Eq,
	PartialOrd,
	Ord,
	Hash,
	Reflect,
	Serialize,
	Deserialize,
)]
pub enum PlayerAction {
	#[actionlike(DualAxis)]
	Move,
	#[actionlike(DualAxis)]
	Look,
	Jump,
	FireA,
	FireB,
	FireC,
	AoE,
	Dash,
	PauseGame,
}

impl PlayerAction {
	pub const ALL: [Self; 9] = [
		Self::Move,
		Self::Look,
		Self::Jump,
		Self::FireA,
		Self::FireB,
		Self::FireC,
		Self::AoE,
		Self::Dash,
		Self::PauseGame,
	];
}

impl ActionExt for PlayerAction {
	fn display_name(&self) -> &'static str {
		match self {
			Self::Move => "Move",
			Self::Look => "Look",
			Self::Jump => "Jump",
			Self::FireA => "Fire A",
			Self::FireB => "Fire B",
			Self::FireC => "Fire C",
			Self::AoE => "AoE",
			Self::Dash => "Dash",
			Self::PauseGame => "Pause",
		}
	}

	fn default_mappings() -> InputMap<Self> {
		use KeyCode::*;
		use PlayerAction::*;
		InputMap::new([
			(Jump, Space),
			(Jump, Backspace),
			(FireA, ShiftRight),
			(FireB, Enter),
			(FireC, ControlRight),
			(FireC, KeyQ),
			(AoE, KeyE),
			(AoE, PageUp),
			(Dash, ShiftLeft),
			(PauseGame, Escape),
		]).with_multiple([
			(FireA, MouseButton::Left),
			(FireB, MouseButton::Right),
			(FireC, MouseButton::Middle),
			(Dash, MouseButton::Other(9)),
		]).with_multiple([
			(Jump, GamepadButton::South),
			(FireA, GamepadButton::RightTrigger2),
			(FireB, GamepadButton::LeftTrigger2),
			(FireC, GamepadButton::LeftTrigger),
			(AoE, GamepadButton::RightTrigger),
			(Dash, GamepadButton::West),
			(PauseGame, GamepadButton::Start),
		]).with_dual_axis(Move, VirtualDPad::wasd())
			.with_dual_axis(Look, VirtualDPad::arrow_keys())
			.with_dual_axis(Move, GamepadStick::LEFT)
			.with_dual_axis(Look, GamepadStick::RIGHT)
			.with_dual_axis(Look, MouseMove::default())
	}
	
	fn all() -> impl Iterator<Item=Self> {
		Self::ALL.into_iter()
	}
}

impl std::fmt::Display for PlayerAction {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		f.write_str(self.display_name())
	}
}

#[derive(Component, Resource, Reflect, Default, Debug, Clone, Deref, DerefMut)]
pub struct InputStorageTimer(Timer);

pub fn look_input(
	mut player_q: Query<(&ActionState<PlayerAction>, &mut CtrlVel, &BelongsToPlayer, &LookSensitivity)>,
	mut camera_pivot_q: Query<
		(&mut Transform, &mut CameraVertSlider, &BelongsToPlayer),
		WithVariant<CamPivot>,
	>,
	t: Res<Time>,
) {
	let dt = (TAU / 0.5/* seconds to max angvel */) * t.delta_secs();
	for (action_state, mut vel, player_id, sens) in player_q.iter_mut() {
		let mut input = Vec2::ZERO;
		if let Some(look) = action_state.dual_axis_data(&PlayerAction::Look) {
			input += look.pair;
		};
		input = input.clamp_length_max(1.0);

		if input.x.abs() > f32::EPSILON {
			vel.angvel.z = (vel.angvel.z - (input.x * sens.x * dt)).clamp(-std::f32::consts::PI / crate::DT, std::f32::consts::PI / crate::DT);
		} else {
			vel.angvel.z *= 0.8
		}

		let (mut xform, mut slider) = camera_pivot_q
			.iter_mut()
			.find_map(|(xform, slider, owner)| (owner == player_id).then_some((xform, slider)))
			.unwrap();
		
		if input.y.abs() > f32::EPSILON {
			slider.0 = (slider.0 + input.y * sens.y * dt).clamp(0.0, 1.0);
		}
		let angle = (-FRAC_PI_2).lerp(FRAC_PI_2 * 0.9, slider.0);
		xform.rotation = xform.rotation.slerp(Quat::from_rotation_x(angle), dt);
	}
}

pub fn movement_input(
	mut q: Query<(&ActionState<PlayerAction>, &mut CtrlVel)>,
	params: Res<PlayerParams>,
	t: Res<Time>,
) {
	for (action_state, mut ctrl_vel) in &mut q {
		let mut input = Vec2::ZERO;
		let dt = t.delta_secs();
		
		if let Some(data) = action_state.dual_axis_data(&PlayerAction::Move) {			input += data.pair;
		}
		input = input.clamp_length_max(1.0);

		let Vec2 { x, y } = ctrl_vel.linvel.xy().lerp(
			input * params.phys.max_speed,
			params.phys.accel * dt,
		);

		// Only trigger change detection if actually changed
		if ctrl_vel.linvel.x != x {
			ctrl_vel.linvel.x = x
		}
		if ctrl_vel.linvel.y != y {
			ctrl_vel.linvel.y = y
		}
	}
}
