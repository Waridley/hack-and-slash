use super::input::PlayerAction;
use crate::player::input::PlayerAction::*;
use bevy::{ecs::query::WorldQuery, prelude::*};
use leafwing_input_manager::prelude::*;
use serde::{Deserialize, Serialize};

/// Player-specific preferences.
/// Should be shared across devices, but may differ between local players.
#[derive(Bundle, Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PlayerPrefs {
	pub invert_camera: InvertCamera,
	pub fov: Fov,
	pub input_map: InputMap<PlayerAction>,
	pub sens: LookSensitivity,
}

impl<'w> From<PlayerPrefsQuery<'w>> for PlayerPrefs {
	fn from(value: PlayerPrefsQuery<'w>) -> Self {
		Self {
			invert_camera: *value.invert_camera,
			fov: *value.fov,
			input_map: value.input_map.clone(),
			sens: *value.sens,
		}
	}
}

#[derive(WorldQuery)]
#[world_query(mutable, derive(Debug))]
pub struct PlayerPrefsQuery<'w> {
	pub invert_camera: &'w mut InvertCamera,
	pub fov: &'w mut Fov,
	pub input_map: &'w mut InputMap<PlayerAction>,
	pub sens: &'w mut LookSensitivity,
}

impl Default for PlayerPrefs {
	fn default() -> Self {
		let mut input_map = InputMap::new([
			(KeyCode::Back, Jump),
			(KeyCode::Space, Jump),
			(KeyCode::ShiftLeft, Dash),
			(KeyCode::ShiftRight, FireA),
			(KeyCode::E, AoE),
			(KeyCode::PageUp, AoE),
			(KeyCode::Escape, Pause),
		]);
		input_map.insert_multiple([
			(DualAxis::left_stick(), Move),
			(DualAxis::right_stick(), Look),
		]);
		input_map.insert_multiple([
			(VirtualDPad::wasd(), Move),
			(VirtualDPad::arrow_keys(), Look),
		]);
		input_map.insert_multiple([
			(GamepadButtonType::South, Jump),
			(GamepadButtonType::West, Dash),
			(GamepadButtonType::RightTrigger2, FireA),
			(GamepadButtonType::RightTrigger, AoE),
			(GamepadButtonType::Start, Pause),
		]);
		input_map.insert_multiple([(MouseButton::Left, FireA), (MouseButton::Other(9), Dash)]);
		Self {
			invert_camera: default(),
			fov: default(),
			input_map,
			sens: default(),
		}
	}
}

#[derive(Component, Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct InvertCamera {
	x: bool,
	y: bool,
}

impl Default for InvertCamera {
	fn default() -> Self {
		// Bwahahaha!
		Self { x: true, y: true }
	}
}

#[derive(Component, Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Fov(f32);

impl Default for Fov {
	fn default() -> Self {
		Self(90.0)
	}
}

#[derive(Component, Debug, Clone, Copy, PartialEq, Deref, DerefMut, Serialize, Deserialize)]
pub struct LookSensitivity(Vec2);

impl Default for LookSensitivity {
	fn default() -> Self {
		Self(Vec2::new(-0.005, -0.005))
	}
}
