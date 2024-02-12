use bevy::app::AppExit;
use bevy::log::{error, info};
use bevy::prelude::{Deref, DerefMut, EventWriter, Events, ResMut, Resource};
use bevy::utils::HashMap;
use colored::Colorize;

use sond_has_engine::testing::*;

pub fn main() {
	let mut app = app();
	for plugin in TESTS.iter().copied() {
		let name = plugin(&mut app);
		info!(
			"{}",
			TestEvent {
				name,
				status: TestStatus::Running
			}
		);
		app.world
			.resource_mut::<RunningTests>()
			.insert(name, TestStatus::Running);
	}
	app.run();
}
