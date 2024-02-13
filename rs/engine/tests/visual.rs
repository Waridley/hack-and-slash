use bevy::log::info;

use sond_has_engine::testing::*;

pub fn main() {
	let mut app = new_test_app();
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
