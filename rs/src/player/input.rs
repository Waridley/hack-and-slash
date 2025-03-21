use crate::{
	player::{
		ctrl::CtrlVel, player_entity::CamPivot, prefs::AimSensitivity, tune::PlayerParams,
		BelongsToPlayer,
	},
	terminal_velocity,
};
use bevy::prelude::*;
use engine::{input::ActionExt, util::LerpSmoothing};
use enum_components::WithVariant;
use leafwing_input_manager::{
	action_diff::{ActionDiff, ActionDiffEvent},
	prelude::*,
	systems::generate_action_diffs,
};
use serde::{Deserialize, Serialize};
use std::{f32::consts::FRAC_PI_2, fmt::Formatter, time::Duration};

#[derive(SystemSet, Copy, Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InputSystems;

pub fn plugin(app: &mut App) -> &mut App {
	app.add_plugins((InputManagerPlugin::<PlayerAction>::default(),))
		.add_event::<ActionDiffEvent<PlayerAction>>()
		.add_systems(First, setup)
		.add_systems(
			Update,
			(
				look_input,
				movement_input.run_if(resource_exists::<PlayerParams>),
			)
				.before(terminal_velocity)
				.in_set(InputSystems),
		)
		.add_systems(PostUpdate, generate_action_diffs::<PlayerAction>)
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
	MotionAim,
	#[actionlike(DualAxis)]
	StickAim,
	Jump,
	FireA,
	FireB,
	FireC,
	AoE,
	Dash,
	PauseGame,
}

impl PlayerAction {
	pub const ALL: [Self; 10] = [
		Self::Move,
		Self::MotionAim,
		Self::StickAim,
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
			Self::MotionAim => "Mouse/motion aiming",
			Self::StickAim => "Control stick aiming",
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
		])
		.with_multiple([
			(FireA, MouseButton::Left),
			(FireB, MouseButton::Right),
			(FireC, MouseButton::Middle),
			(Dash, MouseButton::Forward),
		])
		.with_multiple([
			(Jump, GamepadButton::South),
			(FireA, GamepadButton::RightTrigger2),
			(FireB, GamepadButton::LeftTrigger2),
			(FireC, GamepadButton::LeftTrigger),
			(AoE, GamepadButton::RightTrigger),
			(Dash, GamepadButton::West),
			(PauseGame, GamepadButton::Start),
		])
		.with_dual_axis(Move, VirtualDPad::wasd())
		.with_dual_axis(StickAim, VirtualDPad::arrow_keys())
		.with_dual_axis(Move, GamepadStick::LEFT.with_circle_deadzone(0.1))
		.with_dual_axis(StickAim, GamepadStick::RIGHT.with_circle_deadzone(0.1))
		.with_dual_axis(MotionAim, MouseMove::default())
	}

	fn all() -> impl Iterator<Item = Self> {
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
	mut player_q: Query<(
		Entity,
		&ActionState<PlayerAction>,
		&BelongsToPlayer,
		&AimSensitivity,
	)>,
	mut events: EventReader<ActionDiffEvent<PlayerAction>>,
	mut camera_pivot_q: Query<(&mut Transform, &BelongsToPlayer), WithVariant<CamPivot>>,
	t: Res<Time>,
) {
	let dt = t.delta_secs();
	for (entity, action_state, player_id, sens) in player_q.iter_mut() {
		let mut stick_input = Vec2::ZERO;
		if let Some(look) = action_state.dual_axis_data(&PlayerAction::StickAim) {
			stick_input += look.pair;
		};
		stick_input = stick_input.clamp_length_max(1.0);

		let motion_input = events
			.read()
			.filter(|ev| ev.owner == Some(entity))
			.flat_map(|ev| &ev.action_diffs)
			.filter_map(|diff| {
				if let ActionDiff::DualAxisChanged {
					action: PlayerAction::MotionAim,
					axis_pair,
				} = diff
				{
					Some(*axis_pair)
				} else {
					None
				}
			})
			.sum::<Vec2>();

		let mut xform = camera_pivot_q
			.iter_mut()
			.find_map(|(xform, owner)| (owner == player_id).then_some(xform))
			.unwrap();

		let (yaw, pitch, _) = xform.rotation.to_euler(EulerRot::ZXY);

		let yaw_delta = if motion_input.x.abs() > f32::EPSILON {
			// right-hand rule => + is CCW .: + turns "left" .: needs inverted to make physical sense
			-motion_input.x * sens.motion.x
		} else if stick_input.x.abs() > f32::EPSILON {
			(-stick_input.x * sens.stick.x * dt).clamp(
				-std::f32::consts::PI + crate::EPS,
				std::f32::consts::PI - crate::EPS,
			)
		} else {
			0.0
		};

		let pitch_delta = if motion_input.y.abs() > f32::EPSILON {
			-motion_input.y * sens.motion.y
		} else if stick_input.y.abs() > f32::EPSILON {
			stick_input.y * sens.stick.y * dt
		} else {
			0.0
		};

		let yaw = yaw + yaw_delta;
		let pitch = (pitch + pitch_delta).clamp(-FRAC_PI_2, FRAC_PI_2 * 0.9);

		let new_rot = Quat::from_euler(EulerRot::ZXY, yaw, pitch, 0.0);
		if xform.rotation != new_rot {
			xform.rotation = new_rot;
		}
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

		if let Some(data) = action_state.dual_axis_data(&PlayerAction::Move) {
			input += data.pair;
		}
		input = input.clamp_length_max(1.0);

		let target = input * params.phys.max_speed;
		let Vec2 { x, y } = ctrl_vel
			.linvel
			.xy()
			.exp_decay(target, params.phys.accel, dt);

		// Only trigger change detection if actually changed
		if ctrl_vel.linvel.x != x {
			ctrl_vel.linvel.x = x
		}
		if ctrl_vel.linvel.y != y {
			ctrl_vel.linvel.y = y
		}
	}
}
