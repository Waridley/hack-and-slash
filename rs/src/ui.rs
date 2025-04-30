use crate::ui::{prefs_menu::PrefsMenuPlugin, settings_menu::SettingsMenuPlugin};
use bevy::prelude::*;
use pause_menu::PauseMenuPlugin;

#[cfg(feature = "dev_ui")]
pub mod dbg_ui;
pub mod hud;
pub mod pause_menu;
pub mod prefs_menu;
pub mod settings_menu;

pub struct UiPlugin;

impl Plugin for UiPlugin {
	fn build(&self, app: &mut App) {
		#[cfg(feature = "dev_ui")]
		app.add_plugins(dbg_ui::DbgUiPlugin);

		app.add_plugins((
			hud::HudPlugin,
			PauseMenuPlugin,
			SettingsMenuPlugin,
			PrefsMenuPlugin,
		));
	}
}
