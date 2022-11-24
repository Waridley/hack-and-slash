use bevy::prelude::*;
use serde::{Deserialize, Serialize};

pub struct SettingsPlugin;

impl Plugin for SettingsPlugin {
	fn build(&self, app: &mut App) {
		app.insert_resource(Settings::default());
	}
}

/// Settings that are specific to a local game client.
/// E.g. graphics settings should not be shared between machines.
#[derive(Resource, Default, Debug, Serialize, Deserialize)]
pub struct Settings {
	pub hdr: bool,
}
