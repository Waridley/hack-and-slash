use std::{
	f32::consts::{FRAC_PI_2, TAU},
	time::Duration,
};

use bevy::{
	input::mouse::MouseMotion,
	prelude::{
		GamepadAxisType::{LeftStickX, LeftStickY, RightStickX, RightStickY},
		*,
	},
	window::CursorGrabMode,
};
use enum_components::WithVariant;
use leafwing_input_manager::prelude::*;
use serde::{Deserialize, Serialize};

use engine::ui::UiHovered;

use crate::{
	player::{
		camera::CameraVertSlider, ctrl::CtrlVel, player_entity::CamPivot, prefs::LookSensitivity,
		tune::PlayerParams, BelongsToPlayer,
	},
	terminal_velocity,
};

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
	Move,
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
	pub fn default_mappings() -> InputMap<Self> {
		use KeyCode::*;
		use PlayerAction::*;
		InputMap::new([
			// KB & Mouse
			(Move, UserInput::from(VirtualDPad::wasd())),
			(Look, VirtualDPad::arrow_keys().into()),
			(Jump, Space.into()),
			(Jump, Backspace.into()),
			(FireA, ShiftRight.into()),
			(FireA, MouseButton::Left.into()),
			(FireB, MouseButton::Right.into()),
			(FireB, Enter.into()),
			(FireC, MouseButton::Middle.into()),
			(FireC, ControlRight.into()),
			(FireC, KeyQ.into()),
			(AoE, KeyE.into()),
			(AoE, PageUp.into()),
			(Dash, ShiftLeft.into()),
			(Dash, MouseButton::Other(9).into()),
			(PauseGame, Escape.into()),
			// Controller
			(Move, DualAxis::left_stick().into()),
			(Look, DualAxis::right_stick().into()),
			(Jump, GamepadButtonType::South.into()),
			(FireA, GamepadButtonType::RightTrigger2.into()),
			(FireB, GamepadButtonType::LeftTrigger2.into()),
			(FireC, GamepadButtonType::LeftTrigger.into()),
			(AoE, GamepadButtonType::RightTrigger.into()),
			(Dash, GamepadButtonType::West.into()),
			(PauseGame, GamepadButtonType::Start.into()),
		])
	}
}

#[derive(Component, Resource, Reflect, Default, Debug, Clone, Deref, DerefMut)]
pub struct InputStorageTimer(Timer);

pub fn look_input(
	mut player_q: Query<(&mut CtrlVel, &BelongsToPlayer, &LookSensitivity)>,
	mut camera_pivot_q: Query<
		(&mut Transform, &mut CameraVertSlider, &BelongsToPlayer),
		WithVariant<CamPivot>,
	>,
	kb: Res<ButtonInput<KeyCode>>,
	mut mouse: EventReader<MouseMotion>,
	gp: Res<Gamepads>,
	axes: Res<Axis<GamepadAxis>>,
	windows: Query<&Window>,
	t: Res<Time>,
) {
	for (mut vel, player_id, sens) in player_q.iter_mut() {
		let delta = (TAU / 0.5/* seconds to max angvel */) * t.delta_seconds();

		let mouse = if windows.single().cursor.grab_mode == CursorGrabMode::Locked {
			mouse
				.read()
				.fold(Vec2::ZERO, |acc, delta| acc + delta.delta * **sens)
		} else {
			Vec2::ZERO
		};

		let gp = gp.iter().fold(Vec2::ZERO, |Vec2 { x, y }, gamepad| {
			Vec2::new(
				x + axes
					.get(GamepadAxis {
						gamepad,
						axis_type: RightStickX,
					})
					.unwrap_or_default(),
				y + axes
					.get(GamepadAxis {
						gamepad,
						axis_type: RightStickY,
					})
					.unwrap_or_default(),
			)
		});
		let mut x_input = mouse.x.abs() > 0.0;
		if gp.x.abs() > 0.2 {
			x_input = true;
			vel.angvel.z = (gp.x - (gp.x.signum() * 0.2)) * 1.25 * TAU;
		}
		if kb.pressed(KeyCode::ArrowLeft) {
			x_input = true;
			vel.angvel.z = (-TAU).max(vel.angvel.z - delta);
		}
		if kb.pressed(KeyCode::ArrowRight) {
			x_input = true;
			vel.angvel.z = TAU.min(vel.angvel.z + delta);
		}

		if x_input {
			vel.angvel.z += mouse.x;
		} else {
			vel.angvel.z *= 0.8
		}

		let (mut xform, mut slider) = camera_pivot_q
			.iter_mut()
			.find_map(|(xform, slider, owner)| (owner == player_id).then_some((xform, slider)))
			.unwrap();
		if kb.pressed(KeyCode::ArrowUp) {
			slider.0 = (slider.0 - delta * 0.1).max(0.0);
		}
		if kb.pressed(KeyCode::ArrowDown) {
			slider.0 = (slider.0 + delta * 0.1).min(1.0);
		}
		if gp.y.abs() > 0.2 {
			let y = -gp.y + (0.2 * gp.y.signum()) * 1.25;
			slider.0 = (slider.0 + y * delta * 0.1).clamp(0.0, 1.0);
		}
		if mouse.y.abs() > 0.0 {
			slider.0 = (slider.0 + mouse.y * 0.1).clamp(0.0, 1.0);
		}

		let angle = (-FRAC_PI_2).lerp(FRAC_PI_2 * 0.9, slider.0);
		xform.rotation = xform.rotation.slerp(Quat::from_rotation_x(angle), delta);
	}
}

pub fn movement_input(
	mut q: Query<&mut CtrlVel>,
	kb: Res<ButtonInput<KeyCode>>,
	gp: Res<Gamepads>,
	axes: Res<Axis<GamepadAxis>>,
	params: Res<PlayerParams>,
	t: Res<Time>,
) {
	for mut ctrl_vel in &mut q {
		let (mut x, mut y) = (0.0, 0.0);

		let dt = t.delta_seconds();

		if kb.pressed(KeyCode::KeyW) {
			y += 1.0;
		}
		if kb.pressed(KeyCode::KeyA) {
			x -= 1.0;
		}
		if kb.pressed(KeyCode::KeyS) {
			y -= 1.0;
		}
		if kb.pressed(KeyCode::KeyD) {
			x += 1.0;
		}

		for gamepad in gp.iter() {
			if let Some(val) = axes.get(GamepadAxis {
				gamepad,
				axis_type: LeftStickX,
			}) {
				x += f32::max(val.abs() - 0.2, 0.0) * 1.25 * val.signum();
			}
			if let Some(val) = axes.get(GamepadAxis {
				gamepad,
				axis_type: LeftStickY,
			}) {
				y += f32::max(val.abs() - 0.2, 0.0) * 1.25 * val.signum()
			}
		}

		let Vec2 { x, y } = ctrl_vel.linvel.xy().lerp(
			Vec2 { x, y }.clamp_length_max(1.0) * params.phys.max_speed,
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
