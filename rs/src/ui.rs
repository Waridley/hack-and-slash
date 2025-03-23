use crate::{
	ui::{prefs_menu::PrefsMenuPlugin, settings_menu::SettingsMenuPlugin},
	util::IntoFnPlugin,
};
use bevy::prelude::*;
use pause_menu::PauseMenuPlugin;

#[cfg(feature = "dev_ui")]
pub mod dbg_ui;
pub mod hud;
pub mod pause_menu;
pub mod prefs_menu;
pub mod settings_menu;

pub fn plugin(app: &mut App) -> &mut App {
	#[cfg(feature = "dev_ui")]
	app.add_plugins(dbg_ui::plugin.plugfn());

	app.add_plugins((
		hud::plugin.plugfn(),
		PauseMenuPlugin,
		SettingsMenuPlugin,
		PrefsMenuPlugin,
	))
}
