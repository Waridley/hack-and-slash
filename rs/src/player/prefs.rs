use bevy::{ecs::query::QueryData, prelude::*};
use bevy_pkv::PkvStore;
use engine::util::Angle;
use enum_components::WithVariant;
use leafwing_input_manager::prelude::*;
use serde::{Deserialize, Serialize};

use crate::player::{input::PlayerAction::*, player_entity::Root, BelongsToPlayer};

use super::input::PlayerAction;

pub struct PrefsPlugin;

impl Plugin for PrefsPlugin {
	fn build(&self, app: &mut App) {
		app.register_type::<InvertCamera>()
			.register_type::<Fov>()
			.register_type::<LookSensitivity>()
			.register_type::<CamSmoothing>()
			.add_systems(Update, save);
	}
}

pub fn save(
	q: Query<(PlayerPrefsQueryReadOnly, &BelongsToPlayer), ChangedPrefs>,
	usernames: Query<(&Name, &BelongsToPlayer), WithVariant<Root>>,
	mut pkv: ResMut<PkvStore>,
) {
	for (prefs, owner) in &q {
		let prefs = PlayerPrefs::from(prefs);
		for (name, player) in &usernames {
			let key = format!("{name}.prefs");
			if *player == *owner {
				info!("Saving: {key} => {prefs:#?}");
				pkv.set(key, &prefs).unwrap_or_else(|e| error!("{e}"));
				break;
			}
		}
	}
}

/// Player-specific preferences.
/// Should be shared across devices, but may differ between local players.
#[derive(Bundle, Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(default)]
pub struct PlayerPrefs {
	pub camera: CameraPrefs,
	pub input_map: InputMap<PlayerAction>,
}

pub type ChangedPrefs = Or<(ChangedCameraPrefs, Changed<InputMap<PlayerAction>>)>;

pub type ChangedCameraPrefs = Or<(
	Changed<InvertCamera>,
	Changed<Fov>,
	Changed<LookSensitivity>,
	Changed<CamSmoothing>,
)>;

#[derive(Bundle, Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(default)]
pub struct CameraPrefs {
	pub invert_camera: InvertCamera,
	pub fov: Fov,
	pub sens: LookSensitivity,
	pub smoothing: CamSmoothing,
}

impl<'w> From<PlayerPrefsQueryItem<'w, '_>> for PlayerPrefs {
	fn from(value: PlayerPrefsQueryItem<'w, '_>) -> Self {
		Self {
			camera: CameraPrefs {
				invert_camera: *value.invert_camera,
				fov: *value.fov,
				sens: *value.sens,
				smoothing: *value.cam_smoothing,
			},
			input_map: value.input_map.clone(),
		}
	}
}

impl<'w> From<PlayerPrefsQueryReadOnlyItem<'w, '_>> for PlayerPrefs {
	fn from(value: PlayerPrefsQueryReadOnlyItem<'w, '_>) -> Self {
		Self {
			camera: CameraPrefs {
				invert_camera: *value.invert_camera,
				fov: *value.fov,
				sens: *value.sens,
				smoothing: *value.cam_smoothing,
			},
			input_map: value.input_map.clone(),
		}
	}
}

#[derive(QueryData)]
#[query_data(mutable, derive(Debug))]
pub struct PlayerPrefsQuery<'w> {
	pub invert_camera: &'w mut InvertCamera,
	pub fov: &'w mut Fov,
	pub input_map: &'w mut InputMap<PlayerAction>,
	pub sens: &'w mut LookSensitivity,
	pub cam_smoothing: &'w mut CamSmoothing,
}

impl Default for PlayerPrefs {
	fn default() -> Self {
		use KeyCode::*;
		let mut input_map = InputMap::new([
			(Jump, Backspace),
			(Jump, Space),
			(Dash, ShiftLeft),
			(FireA, ShiftRight),
			(AoE, KeyE),
			(AoE, PageUp),
			(PauseGame, Escape),
		]);
		input_map.insert_multiple([
			(Move, DualAxis::left_stick()),
			(Look, DualAxis::right_stick()),
		]);
		input_map.insert_multiple([
			(Move, VirtualDPad::wasd()),
			(Look, VirtualDPad::arrow_keys()),
		]);
		input_map.insert_multiple([
			(Jump, GamepadButtonType::South),
			(Dash, GamepadButtonType::West),
			(FireA, GamepadButtonType::RightTrigger2),
			(AoE, GamepadButtonType::RightTrigger),
			(PauseGame, GamepadButtonType::Start),
		]);
		input_map.insert_multiple([(FireA, MouseButton::Left), (Dash, MouseButton::Other(9))]);
		Self {
			input_map,
			camera: default(),
		}
	}
}

#[derive(Component, Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Reflect)]
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

#[derive(Component, Debug, Clone, Copy, PartialEq, Serialize, Deserialize, Reflect)]
#[serde(default)]
pub struct Fov(Angle);

impl Default for Fov {
	fn default() -> Self {
		Self(Angle::Deg(45.0))
	}
}

#[derive(
	Component, Debug, Clone, Copy, PartialEq, Deref, DerefMut, Serialize, Deserialize, Reflect,
)]
#[serde(default)]
pub struct LookSensitivity(Vec2);

impl Default for LookSensitivity {
	fn default() -> Self {
		Self(Vec2::new(-0.005, -0.005))
	}
}

#[derive(
	Component, Debug, Clone, Copy, PartialEq, Deref, DerefMut, Serialize, Deserialize, Reflect,
)]
#[serde(default)]
pub struct CamSmoothing(f32);

impl Default for CamSmoothing {
	fn default() -> Self {
		Self(0.25)
	}
}
