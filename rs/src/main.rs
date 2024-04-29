#![cfg_attr(
	all(not(debug_assertions), target_os = "windows"),
	windows_subsystem = "windows"
)]

use bevy::{prelude::*, window::PresentMode};

use sond_has::util::IntoFnPlugin;
pub(crate) use sond_has::*;

#[bevy_main]
pub fn main() {
	let mut app = App::new();
	let default_plugins = DefaultPlugins
		.set(WindowPlugin {
			primary_window: Some(Window {
				title: "Sonday Hack-and-Slash Game".to_string(),
				resizable: true,
				canvas: Some("#game_canvas".into()),
				present_mode: PresentMode::AutoNoVsync,
				..default()
			}),
			..default()
		})
		.set(AssetPlugin {
			#[cfg(feature = "debugging")]
			file_path: dbg!(
				std::env::var("ASSET_DIR").unwrap_or_else(|_| AssetPlugin::default().file_path)
			),
			#[cfg(feature = "debugging")]
			processed_file_path: dbg!(std::env::var("PROC_ASSET_DIR")
				.unwrap_or_else(|_| AssetPlugin::default().processed_file_path)),
			mode: AssetMode::Processed,
			..default()
		});

	app.add_plugins(default_plugins);

	app.add_plugins(game_plugin.plugfn());

	app.run()
}
