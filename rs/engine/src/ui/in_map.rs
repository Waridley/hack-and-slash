use bevy::reflect::prelude::*;
use serde::{Deserialize, Serialize};

pub mod detect;
pub mod icons;

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Reflect)]
#[reflect(Serialize, Deserialize)]
pub enum GamepadSeries {
	#[default]
	Generic,
	Nintendo,
	PlayStation,
	SteamController,
	SteamDeck,
	Xbox,
}

impl GamepadSeries {
	pub fn guess(name: &str) -> Self {
		// TODO: Hard-coded feature flags for consoles

		// TODO: More name mappings
		if name.contains("Nintendo") {
			Self::Nintendo
		} else if ["Xbox", "XB", "Afterglow"]
			.into_iter()
			.any(|s| name.contains(s))
		{
			// "Afterglow" could be other series, but this is the one I have for testing.
			// Probably need a way to let the player choose the series.
			Self::Xbox
		} else if name.contains("PlayStation") || name.contains("PS") {
			Self::PlayStation
		} else if name.contains("Steam") {
			Self::SteamController
		} else {
			Self::Generic
		}
	}
}
