use crate::util;
use bevy::reflect::prelude::*;
use serde::{Deserialize, Serialize};

pub mod detect;
pub mod icons;
pub mod widgets;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Reflect)]
#[reflect(Serialize, Deserialize)]
pub enum Platform {
	Windows,
	Mac,
	Linux,
	Nintendo,
	PlayStation,
	SteamController,
	SteamDeck,
	Xbox,
}

impl Platform {
	pub fn guess(gamepad_name: &str) -> Option<Self> {
		Self::guess_gamepad(gamepad_name).or_else(Self::guess_os)
	}

	pub fn guess_os() -> Option<Self> {
		if util::host_is_windows() {
			Some(Self::Windows)
		} else if util::host_is_mac() {
			Some(Self::Mac)
		} else if util::host_is_linux() {
			Some(Self::Linux)
		} else {
			None
		}
	}

	pub fn guess_gamepad(name: &str) -> Option<Self> {
		// TODO: Hard-coded feature flags for consoles

		// TODO: More name mappings
		if name.contains("Nintendo") {
			Some(Self::Nintendo)
		} else if ["Xbox", "XB", "Afterglow"]
			.into_iter()
			.any(|s| name.contains(s))
		{
			// "Afterglow" could be other series, but this is the one I have for testing.
			// Probably need a way to let the player choose the series.
			Some(Self::Xbox)
		} else if name.contains("PlayStation") || name.contains("PS") {
			Some(Self::PlayStation)
		} else if name.contains("Steam") {
			Some(Self::SteamController)
		} else {
			None
		}
	}
}
