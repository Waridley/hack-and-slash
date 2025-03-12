use crate::player::{player_entity::Root, BelongsToPlayer};
use bevy::{ecs::query::QueryData, prelude::*};
use bevy_pkv::PkvStore;
use engine::{input::ActionExt, ui::UiAction, util::Angle};
use enum_components::WithVariant;
use leafwing_input_manager::prelude::*;
use serde::{Deserialize, Serialize};

use super::input::PlayerAction;

pub struct PrefsPlugin;

impl Plugin for PrefsPlugin {
	fn build(&self, app: &mut App) {
		app.register_type::<InvertCamera>()
			.register_type::<Fov>()
			.register_type::<AimSensitivity>()
			.register_type::<CamSmoothing>()
			.add_systems(Last, save);
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
	pub ui_input_map: InputMap<UiAction>,
}

pub type ChangedPrefs = Or<(
	ChangedCameraPrefs,
	Changed<InputMap<PlayerAction>>,
	Changed<InputMap<UiAction>>,
)>;

pub type ChangedCameraPrefs = Or<(
	Changed<InvertCamera>,
	Changed<Fov>,
	Changed<AimSensitivity>,
	Changed<CamSmoothing>,
)>;

#[derive(Bundle, Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(default)]
pub struct CameraPrefs {
	pub invert_camera: InvertCamera,
	pub fov: Fov,
	pub sens: AimSensitivity,
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
			ui_input_map: value.ui_input_map.clone(),
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
			ui_input_map: value.ui_input_map.clone(),
		}
	}
}

#[derive(QueryData)]
#[query_data(mutable, derive(Debug))]
pub struct PlayerPrefsQuery<'w> {
	pub invert_camera: &'w mut InvertCamera,
	pub fov: &'w mut Fov,
	pub input_map: &'w mut InputMap<PlayerAction>,
	pub ui_input_map: &'w mut InputMap<UiAction>,
	pub sens: &'w mut AimSensitivity,
	pub cam_smoothing: &'w mut CamSmoothing,
}

impl Default for PlayerPrefs {
	fn default() -> Self {
		Self {
			camera: default(),
			input_map: PlayerAction::default_mappings(),
			ui_input_map: UiAction::default_mappings(),
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
	Component, Debug, Clone, Copy, PartialEq, Serialize, Deserialize, Reflect,
)]
#[serde(default)]
pub struct AimSensitivity {
	pub motion: Vec2,
	pub stick: Vec2,
}

impl Default for AimSensitivity {
	fn default() -> Self {
		Self { stick: Vec2::new(-2.0, -2.0), motion: Vec2::new(0.001, 0.001) }
	}
}

#[derive(
	Component, Debug, Clone, Copy, PartialEq, Deref, DerefMut, Serialize, Deserialize, Reflect,
)]
#[serde(default)]
pub struct CamSmoothing(f32);

impl Default for CamSmoothing {
	fn default() -> Self {
		Self(8.0)
	}
}
