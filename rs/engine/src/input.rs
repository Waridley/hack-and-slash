use crate::{
	input::{map::icons::InputIconFileMap, InputState::DetectingBinding},
	util::AppExt,
};
use bevy::{
	input::keyboard::Key,
	prelude::*,
	reflect::{FromType, TypeRegistration},
	state::state::States,
	utils::HashMap,
};
use leafwing_input_manager::prelude::*;
use map::detect;
use serde::{Deserialize, Serialize};
use tiny_bail::prelude::r;

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
					detect::dbg_set_detect_binding_state.before(detect::detect_bindings),
					detect::detect_bindings.run_if(in_state(DetectingBinding)),
					#[cfg(feature = "debugging")]
					detect::dbg_detect_bindings
						.run_if(resource_exists::<InputIconFileMap>)
						.after(detect::detect_bindings),
				),
			);
	}

	fn finish(&self, app: &mut App) {
		let mut reg = TypeRegistration::of::<GamepadAxis>();
		reg.insert(<ReflectDeserialize as FromType<GamepadAxis>>::from_type());
		let registry = app.world_mut().resource_mut::<AppTypeRegistry>();
		let mut registry = registry.internal.write().unwrap();
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

pub trait ActionExt: Actionlike {
	fn display_name(&self) -> &'static str;
	fn default_mappings() -> InputMap<Self>;
	fn reset_to_default(&self, input_map: &mut InputMap<Self>) {
		let default = Self::default_mappings();
		match self.input_control_kind() {
			InputControlKind::Button => {
				let default = default.get_buttonlike(self);
				if let Some(default) = default {
					debug!(action = self.display_name(), ?default);
					let bindings = input_map.get_buttonlike_mut(self);
					let bindings = if let Some(bindings) = bindings {
						bindings
					} else {
						input_map.insert_one_to_many(self.clone(), std::iter::empty::<KeyCode>());
						r!(input_map.get_buttonlike_mut(self))
					};
					bindings.clear();
					bindings.clone_from(default);
				} else {
					input_map.clear_action(self);
				}
			}
			InputControlKind::Axis => {
				let default = default.get_axislike(self);
				if let Some(default) = default {
					debug!(action = self.display_name(), ?default);
					let bindings = input_map.get_axislike_mut(self);
					let bindings = if let Some(bindings) = bindings {
						bindings
					} else {
						// Hack, will be cleared, need to initialize bindings list
						input_map.insert_axis(self.clone(), GamepadAxis::LeftStickX);
						r!(input_map.get_axislike_mut(self))
					};
					bindings.clear();
					bindings.clone_from(default);
				} else {
					input_map.clear_action(self);
				}
			}
			InputControlKind::DualAxis => {
				let default = default.get_dual_axislike(self);
				if let Some(default) = default {
					debug!(action = self.display_name(), ?default);
					let bindings = input_map.get_dual_axislike_mut(self);
					let bindings = if let Some(bindings) = bindings {
						bindings
					} else {
						input_map.insert_dual_axis(self.clone(), GamepadStick::LEFT);
						r!(input_map.get_dual_axislike_mut(self))
					};
					bindings.clear();
					bindings.clone_from(default);
				} else {
					input_map.clear_action(self);
				}
			}
			InputControlKind::TripleAxis => {
				let default = default.get_triple_axislike(self);
				if let Some(default) = default {
					debug!(action = self.display_name(), ?default);
					let bindings = input_map.get_triple_axislike_mut(self);
					let bindings = if let Some(bindings) = bindings {
						bindings
					} else {
						input_map.insert_triple_axis(
							self.clone(),
							VirtualDPad3D {
								up: Box::new(KeyCode::Escape),
								down: Box::new(KeyCode::Escape),
								left: Box::new(KeyCode::Escape),
								right: Box::new(KeyCode::Escape),
								forward: Box::new(KeyCode::Escape),
								backward: Box::new(KeyCode::Escape),
							},
						);
						r!(input_map.get_triple_axislike_mut(self))
					};
					bindings.clear();
					bindings.clone_from(default);
				} else {
					input_map.clear_action(self);
				}
			}
		}
	}
	fn all() -> impl Iterator<Item = Self>;
}
