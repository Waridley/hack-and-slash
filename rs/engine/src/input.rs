use bevy::utils::HashSet;
use bevy::{
	input::{
		gamepad::{GamepadAxisChangedEvent, GamepadButtonChangedEvent, GamepadButtonInput},
		keyboard::KeyboardInput,
		mouse::{MouseButtonInput, MouseMotion, MouseWheel},
	},
	prelude::*,
};
use leafwing_input_manager::{buttonlike::MouseMotionDirection, prelude::*, user_input::InputKind};
use serde::{Deserialize, Serialize};

use InputState::{InGame, InMenu};

use crate::{
	input::InputState::DetectingBinding,
	util::{AppExt, StateStack},
};

pub struct InputPlugin;

impl Plugin for InputPlugin {
	fn build(&self, app: &mut App) {
		app.init_state_stack::<InputState>()
			.add_event::<ToBind>()
			.add_systems(
				Update,
				(
					detect_bindings.run_if(in_state(DetectingBinding)),
					#[cfg(feature = "debugging")]
					dbg_detect_bindings.after(detect_bindings),
				),
			);
	}
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default, States)]
pub enum InputState {
	#[default]
	InMenu,
	InGame,
	DetectingBinding,
}

/// What the user input when prompted to bind an action. May be a single input,
/// in which case the binding should be treated as `UserInput::Single`, or it
/// may be a chord.
///
/// If a `VirtualDPad` is being bound, detecting and grouping separate
/// directions must be done by the recipient of these events.
///
/// Gamepad events should always be paired with `Some(Gamepad)`, and other
/// events should be paired with `None`. Currently, an `InputMap` can only
/// accept a single associated gamepad. How to handle buttons from different
/// gamepads is up to the recipient of these events, i.e., only accepting
/// buttons on a single associated gamepad to prevent other players from
/// interfering, discarding the gamepad to accept inputs on *all* gamepads,
/// or somehow manually associating multiple gamepads with an `InputMap`.
#[derive(Event, Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Reflect)]
pub struct ToBind(pub HashSet<(InputKind, Option<Gamepad>)>);

pub fn detect_bindings(
	mut gamepad_buttons: EventReader<GamepadButtonInput>,
	mut gamepad_button_axes: EventReader<GamepadButtonChangedEvent>,
	mut gamepad_axes: EventReader<GamepadAxisChangedEvent>,
	mut keys: EventReader<KeyboardInput>,
	mut mouse_buttons: EventReader<MouseButtonInput>,
	mut mouse_wheel: EventReader<MouseWheel>,
	mut mouse_motion: EventReader<MouseMotion>,
	mut tx: EventWriter<ToBind>,
	mut mouse_accum: Local<Vec2>,
	mut wheel_accum: Local<Vec2>,
	mut curr_chord: Local<HashSet<(InputKind, Option<Gamepad>)>>,
) {
	let mut finalize =
		|chord: &mut HashSet<(InputKind, Option<Gamepad>)>, m: &mut Vec2, w: &mut Vec2| {
			*m = Vec2::ZERO;
			*w = Vec2::ZERO;
			let binding = ToBind(std::mem::take(chord));
			info!("{binding:?}");
			tx.send(binding);
		};

	// Only finalize binding when the user *releases* at least one button.
	// This allows chords to be bound, whether physically input by the user,
	// or automatically sent by macro keys for example.

	for btn in gamepad_buttons.read() {
		let entry = (btn.button.button_type.into(), Some(btn.button.gamepad));
		trace!("{entry:?}");
		if btn.state.is_pressed() {
			curr_chord.insert(entry);
		} else if curr_chord.contains(&entry) {
			finalize(&mut *curr_chord, &mut mouse_accum, &mut wheel_accum);
		}
	}

	// Thresholds are higher than usual to make sure the user really wants the given input.

	for btn in gamepad_button_axes.read() {
		let entry = (btn.button_type.into(), Some(btn.gamepad));
		trace!("{entry:?}");
		if btn.value > 0.7 {
			curr_chord.insert(entry);
		} else if curr_chord.contains(&entry) {
			finalize(&mut *curr_chord, &mut mouse_accum, &mut wheel_accum);
		}
	}

	for axis in gamepad_axes.read() {
		let entry = (
			SingleAxis::from_value(axis.axis_type, axis.value).into(),
			Some(axis.gamepad),
		);
		trace!("{entry:?}");
		if axis.value.abs() > 0.7 {
			curr_chord.insert(entry);
		} else if curr_chord.contains(&entry) {
			finalize(&mut *curr_chord, &mut mouse_accum, &mut wheel_accum);
		}
	}

	for key in keys.read() {
		let entry = (key.key_code.into(), None);
		trace!("{entry:?}");
		if key.state.is_pressed() {
			curr_chord.insert(entry);
		} else if curr_chord.contains(&entry) {
			finalize(&mut *curr_chord, &mut mouse_accum, &mut wheel_accum);
		}
	}

	for btn in mouse_buttons.read() {
		let entry = (btn.button.into(), None);
		trace!("{entry:?}");
		if btn.state.is_pressed() {
			curr_chord.insert(entry);
		} else if curr_chord.contains(&entry) {
			finalize(&mut *curr_chord, &mut mouse_accum, &mut wheel_accum);
		}
	}

	for wheel in mouse_wheel.read() {
		wheel_accum.x += wheel.x;
		wheel_accum.y += wheel.y;
		trace!("{wheel_accum:?}");
		if wheel_accum.x >= 3.0 {
			curr_chord.insert((MouseWheelDirection::Right.into(), None));
			finalize(&mut *curr_chord, &mut mouse_accum, &mut wheel_accum);
		} else if wheel_accum.x <= -3.0 {
			curr_chord.insert((MouseWheelDirection::Left.into(), None));
			finalize(&mut *curr_chord, &mut mouse_accum, &mut wheel_accum);
		}
		if wheel_accum.y >= 3.0 {
			curr_chord.insert((MouseWheelDirection::Up.into(), None));
			finalize(&mut *curr_chord, &mut mouse_accum, &mut wheel_accum);
		} else if wheel_accum.y <= -3.0 {
			curr_chord.insert((MouseWheelDirection::Down.into(), None));
			finalize(&mut *curr_chord, &mut mouse_accum, &mut wheel_accum);
		}
	}

	for motion in mouse_motion.read() {
		*mouse_accum += motion.delta;
		trace!("{mouse_accum:?}");
		// FIXME: Probably scale by sensitivity
		if mouse_accum.x > 500.0 {
			curr_chord.insert((MouseMotionDirection::Right.into(), None));
			finalize(&mut *curr_chord, &mut mouse_accum, &mut wheel_accum);
		} else if mouse_accum.x < -500.0 {
			curr_chord.insert((MouseMotionDirection::Left.into(), None));
			finalize(&mut *curr_chord, &mut mouse_accum, &mut wheel_accum);
		}
		if mouse_accum.y > 500.0 {
			curr_chord.insert((MouseMotionDirection::Down.into(), None));
			finalize(&mut *curr_chord, &mut mouse_accum, &mut wheel_accum);
		} else if mouse_accum.y < -500.0 {
			curr_chord.insert((MouseMotionDirection::Up.into(), None));
			finalize(&mut *curr_chord, &mut mouse_accum, &mut wheel_accum);
		}
	}
}

#[cfg(feature = "debugging")]
pub fn dbg_detect_bindings(
	keys: Res<ButtonInput<KeyCode>>,
	mut stack: ResMut<StateStack<InputState>>,
	mut rx: EventReader<ToBind>,
) {
	if keys.just_released(KeyCode::KeyB) {
		match stack.last() {
			Some(InMenu | InGame) => stack.push(DetectingBinding),
			Some(DetectingBinding) => {
				stack.pop();
			}
			None => {
				error!("InputState stack was somehow empty.");
				stack.push(InputState::default());
			}
		}
	}

	for event in rx.read() {
		dbg!(event);
	}
}
