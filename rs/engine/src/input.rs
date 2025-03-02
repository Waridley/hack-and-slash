use bevy::{
	input::{
		gamepad::{GamepadAxisChangedEvent, GamepadButtonChangedEvent},
		keyboard::{Key, KeyboardInput},
		mouse::{MouseButtonInput, MouseMotion, MouseWheel},
		ButtonState,
	},
	prelude::*,
	state::state::States,
	utils::HashMap,
};
use bevy::reflect::{FromType, TypeRegistration};
use leafwing_input_manager::prelude::*;
use leafwing_input_manager::prelude::updating::CentralInputStore;
use serde::{Deserialize, Serialize};
use tiny_bail::prelude::r;
use crate::{
	input::InputState::DetectingBinding,
	util::{AppExt, StateStack},
};
use crate::input::map::icons::{IconPathKey, InputIconFileMap};

pub mod map;

pub struct InputPlugin;

impl Plugin for InputPlugin {
	fn build(&self, app: &mut App) {
		app.add_plugins((map::detect::DetectBindingPopupPlugin,))
			.init_state_stack::<InputState>()
			.init_resource::<CurrentChord>()
			.register_axislike_input::<GamepadAxis>()
			.register_type::<InputIconFileMap>()
			.add_event::<ToBind>()
			.add_systems(
				First, // Consume inputs while detecting bindings
				(
					#[cfg(feature = "debugging")]
					dbg_set_detect_binding_state.before(detect_bindings),
					detect_bindings.run_if(in_state(DetectingBinding)),
					#[cfg(feature = "debugging")]
					dbg_detect_bindings.run_if(resource_exists::<InputIconFileMap>).after(detect_bindings),
				),
			);
	}
	
	fn finish(&self, app: &mut App) {
		let mut reg = TypeRegistration::of::<GamepadAxis>();
		reg.insert(<ReflectDeserialize as FromType<GamepadAxis>>::from_type());
		let mut registry = app.world_mut()
			.resource_mut::<AppTypeRegistry>();
		let mut registry = registry
			.internal
			.write()
			.unwrap();
		registry.overwrite_registration(reg);
	}
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default, States)]
pub enum InputState {
	#[default]
	InGame,
	InMenu,
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
///
/// `KeyboardInput` events will be mapped to `Some(Key)`, which should be
/// used for the purpose of icon selection.
#[derive(Event, Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Reflect)]
pub struct ToBind(pub CurrentChord);

#[derive(Resource, Default, Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Reflect)]
pub struct CurrentChord(HashMap<ChordEntry, Option<Key>>);

impl CurrentChord {
	pub fn iter(&self) -> impl Iterator<Item = (&ChordEntry, &Option<Key>)> {
		self.0.iter()
	}

	pub fn clear(&mut self) {
		self.0.clear()
	}

	pub fn insert(&mut self, entry: ChordEntry, key: Option<Key>) {
		self.0.insert(entry, key);
	}

	pub fn contains_key(&self, key: &ChordEntry) -> bool {
		self.0.contains_key(key)
	}

	pub fn len(&self) -> usize {
		self.0.len()
	}

	pub fn is_empty(&self) -> bool {
		self.0.is_empty()
	}
}

#[derive(Reflect, Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum SerializableUserInputWrapper {
	Button(Box<dyn Buttonlike>),
	Axis(Box<dyn Axislike>),
	DualAxis(Box<dyn DualAxislike>),
	TripleAxis(Box<dyn TripleAxislike>),
}

impl From<UserInputWrapper> for SerializableUserInputWrapper {
	fn from(wrapper: UserInputWrapper) -> Self {
		match wrapper {
			UserInputWrapper::Button(input) => Self::Button(input),
			UserInputWrapper::Axis(input) => Self::Axis(input),
			UserInputWrapper::DualAxis(input) => Self::DualAxis(input),
			UserInputWrapper::TripleAxis(input) => Self::TripleAxis(input),
		}
	}
}

impl From<SerializableUserInputWrapper> for UserInputWrapper {
	fn from(wrapper: SerializableUserInputWrapper) -> Self {
		match wrapper {
			SerializableUserInputWrapper::Button(input) => Self::Button(input),
			SerializableUserInputWrapper::Axis(input) => Self::Axis(input),
			SerializableUserInputWrapper::DualAxis(input) => Self::DualAxis(input),
			SerializableUserInputWrapper::TripleAxis(input) => Self::TripleAxis(input),
		}
	}
}

pub type ChordEntry = (SerializableUserInputWrapper, Option<Entity>);

pub fn detect_bindings(
	mut gamepad_buttons: ResMut<Events<GamepadButtonChangedEvent>>,
	mut gamepad_axes: ResMut<Events<GamepadAxisChangedEvent>>,
	mut keys: ResMut<Events<KeyboardInput>>,
	mut mouse_buttons: ResMut<Events<MouseButtonInput>>,
	mut mouse_wheel: ResMut<Events<MouseWheel>>,
	mut mouse_motion: ResMut<Events<MouseMotion>>,
	mut tx: EventWriter<ToBind>,
	mut curr_chord: ResMut<CurrentChord>,
	mut mouse_accum: Local<Vec2>,
	mut wheel_accum: Local<Vec2>,
) {
	let mut finalize = |chord: &mut CurrentChord, m: &mut Vec2, w: &mut Vec2| {
		*m = Vec2::ZERO;
		*w = Vec2::ZERO;
		let binding = ToBind(std::mem::take(chord));
		info!("{binding:?}");
		tx.send(binding);
	};

	// Only finalize binding when the user *releases* at least one button.
	// This allows chords to be bound, whether physically input by the user,
	// or automatically sent by macro keys for example.

	for ev in gamepad_buttons.drain() {
		let entry = (UserInputWrapper::Button(Box::new(ev.button)).into(), Some(ev.entity));
		trace!("{entry:?}");
		if ev.state.is_pressed() {
			curr_chord.insert(entry, None);
		} else if curr_chord.contains_key(&entry.into()) {
			finalize(&mut curr_chord, &mut mouse_accum, &mut wheel_accum);
		}
	}

	// Thresholds are higher than usual to make sure the user really wants the given input.

	for axis in gamepad_axes.drain() {
		let entry = (
			UserInputWrapper::Axis(Box::new(axis.axis)).into(),
			Some(axis.entity),
		);
		trace!("{entry:?}");
		if axis.value.abs() > 0.7 {
			curr_chord.insert(entry, None);
		} else if curr_chord.contains_key(&entry) {
			finalize(&mut curr_chord, &mut mouse_accum, &mut wheel_accum);
		}
	}

	for key in keys.drain() {
		let entry = (UserInputWrapper::Button(Box::new(key.key_code)).into(), None);
		trace!("{entry:?}");
		if key.state.is_pressed() {
			curr_chord.insert(entry, Some(key.logical_key));
		} else if curr_chord.contains_key(&entry) {
			finalize(&mut curr_chord, &mut mouse_accum, &mut wheel_accum);
		}
	}

	for btn in mouse_buttons.drain() {
		let entry = (UserInputWrapper::Button(Box::new(btn.button)).into(), None);
		trace!("{entry:?}");
		if btn.state.is_pressed() {
			curr_chord.insert(entry, None);
		} else if curr_chord.contains_key(&entry) {
			finalize(&mut curr_chord, &mut mouse_accum, &mut wheel_accum);
		}
	}

	for wheel in mouse_wheel.drain() {
		wheel_accum.x += wheel.x;
		wheel_accum.y += wheel.y;
		trace!("{wheel_accum:?}");
		if wheel_accum.x >= 3.0 {
			curr_chord.insert((UserInputWrapper::Button(Box::new(MouseScrollDirection::RIGHT)).into(), None), None);
			finalize(&mut curr_chord, &mut mouse_accum, &mut wheel_accum);
		} else if wheel_accum.x <= -3.0 {
			curr_chord.insert((UserInputWrapper::Button(Box::new(MouseScrollDirection::LEFT)).into(), None), None);
			finalize(&mut curr_chord, &mut mouse_accum, &mut wheel_accum);
		}
		if wheel_accum.y >= 3.0 {
			curr_chord.insert((UserInputWrapper::Button(Box::new(MouseScrollDirection::UP)).into(), None), None);
			finalize(&mut curr_chord, &mut mouse_accum, &mut wheel_accum);
		} else if wheel_accum.y <= -3.0 {
			curr_chord.insert((UserInputWrapper::Button(Box::new(MouseScrollDirection::DOWN)).into(), None), None);
			finalize(&mut curr_chord, &mut mouse_accum, &mut wheel_accum);
		}
	}

	for motion in mouse_motion.drain() {
		*mouse_accum += motion.delta;
		trace!("{mouse_accum:?}");
		// FIXME: Probably scale by sensitivity
		if mouse_accum.x > 500.0 {
			curr_chord.insert((UserInputWrapper::Button(Box::new(MouseMoveDirection::RIGHT)).into(), None), None);
			finalize(&mut curr_chord, &mut mouse_accum, &mut wheel_accum);
		} else if mouse_accum.x < -500.0 {
			curr_chord.insert((UserInputWrapper::Button(Box::new(MouseMoveDirection::LEFT)).into(), None), None);
			finalize(&mut curr_chord, &mut mouse_accum, &mut wheel_accum);
		}
		if mouse_accum.y > 500.0 {
			curr_chord.insert((UserInputWrapper::Button(Box::new(MouseMoveDirection::DOWN)).into(), None), None);
			finalize(&mut curr_chord, &mut mouse_accum, &mut wheel_accum);
		} else if mouse_accum.y < -500.0 {
			curr_chord.insert((UserInputWrapper::Button(Box::new(MouseMoveDirection::UP)).into(), None), None);
			finalize(&mut curr_chord, &mut mouse_accum, &mut wheel_accum);
		}
	}
}

#[cfg(feature = "debugging")]
pub fn dbg_set_detect_binding_state(
	mut stack: ResMut<StateStack<InputState>>,
	mut keys: EventReader<KeyboardInput>,
	mut curr_chord: ResMut<CurrentChord>,
) {
	for key in keys.read() {
		if key.key_code == KeyCode::KeyB && key.state == ButtonState::Released {
			match stack.last() {
				Some(DetectingBinding) => {
					stack.pop();
				}
				Some(_) => stack.push(DetectingBinding),
				None => {
					error!("InputState stack was somehow empty.");
					stack.push(InputState::default());
				}
			}
			curr_chord.clear()
		}
	}
}

#[cfg(feature = "debugging")]
pub fn dbg_detect_bindings(
	mut rx: EventReader<ToBind>,
	gamepads: Query<(Option<&Name>, &Gamepad)>,
	icon_map: Res<InputIconFileMap>,
) {
	use crate::input::map::{icons::*, Platform};
	for event in rx.read() {
		for ((input, gp), logical_key) in event.0.iter() {
			let icons = if let Some(icon) = logical_key.as_ref() {
				key(icon).map(UserInputIcons::simple).unwrap_or_default()
			} else {
				UserInputIcons::from_user_input(
					input.clone().into(),
					gp.and_then(|gp| gamepads.get(gp).ok())
						.and_then(|(name, _)| name)
						.map(Name::as_str)
						.and_then(Platform::guess_gamepad),
					&*icon_map,
				)
			};

			info!(input = ?input, gamepad = ?gp, icons = ?icons);
		}
	}
}

pub trait ActionExt: Actionlike {
	fn display_name(&self) -> &'static str;
	fn default_mappings() -> InputMap<Self>;
	fn reset_to_default(&self, input_map: &mut InputMap<Self>) {
		let default = Self::default_mappings();
		match self.input_control_kind() {
			InputControlKind::Button => {
				let default = default.get_buttonlike(self);
				if let Some(default) = default {
					debug!(action=self.display_name(), ?default);
					let bindings = input_map.get_buttonlike_mut(self);
					let bindings = if let Some(bindings) = bindings {
						bindings
					} else {
						input_map.insert_one_to_many(self.clone(), std::iter::empty::<KeyCode>());
						r!(input_map.get_buttonlike_mut(self))
					};
					bindings.clear();
					bindings.clone_from(&default);
				} else {
					input_map.clear_action(self);
				}
			}
			InputControlKind::Axis => {
				let default = default.get_axislike(self);
				if let Some(default) = default {
					debug!(action=self.display_name(), ?default);
					let bindings = input_map.get_axislike_mut(self);
					let bindings = if let Some(bindings) = bindings {
						bindings
					} else {
						// Hack, will be cleared, need to initialize bindings list
						input_map.insert_axis(self.clone(), GamepadAxis::LeftStickX);
						r!(input_map.get_axislike_mut(self))
					};
					bindings.clear();
					bindings.clone_from(&default);
				} else {
					input_map.clear_action(self);
				}
			}
			InputControlKind::DualAxis => {
				let default = default.get_dual_axislike(self);
				if let Some(default) = default {
					debug!(action=self.display_name(), ?default);
					let bindings = input_map.get_dual_axislike_mut(self);
					let bindings = if let Some(bindings) = bindings {
						bindings
					} else {
						input_map.insert_dual_axis(self.clone(), GamepadStick::LEFT);
						r!(input_map.get_dual_axislike_mut(self))
					};
					bindings.clear();
					bindings.clone_from(&default);
				} else {
					input_map.clear_action(self);
				}
			}
			InputControlKind::TripleAxis => {
				let default = default.get_triple_axislike(self);
				if let Some(default) = default {
					debug!(action=self.display_name(), ?default);
					let bindings = input_map.get_triple_axislike_mut(self);
					let bindings = if let Some(bindings) = bindings {
						bindings
					} else {
						input_map.insert_triple_axis(self.clone(), VirtualDPad3D {
							up: Box::new(KeyCode::Escape),
							down: Box::new(KeyCode::Escape),
							left: Box::new(KeyCode::Escape),
							right: Box::new(KeyCode::Escape),
							forward: Box::new(KeyCode::Escape),
							backward: Box::new(KeyCode::Escape),
						});
						r!(input_map.get_triple_axislike_mut(self))
					};
					bindings.clear();
					bindings.clone_from(&default);
				} else {
					input_map.clear_action(self);
				}
			}
		}
	}
	fn all() -> impl Iterator<Item = Self>;
}
