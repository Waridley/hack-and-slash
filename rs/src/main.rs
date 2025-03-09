#![cfg_attr(
	all(not(debug_assertions), target_os = "windows"),
	windows_subsystem = "windows"
)]

use bevy::{prelude::*, window::PresentMode};

pub(crate) use sond_has::*;

#[bevy_main]
pub fn main() -> AppExit {
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
			mode: AssetMode::Processed,
			..default()
		});

	app.add_plugins(default_plugins);

	app.add_plugins(GamePlugin);

	app.run()
}
