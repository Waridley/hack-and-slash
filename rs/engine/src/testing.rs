use std::error::Error;
use std::fmt::{Display, Formatter};

use bevy::app::{App, AppExit, Startup, Update};
use bevy::log::{error, info, warn};
use bevy::math::{Quat, Vec3};
use bevy::prelude::*;
use bevy::utils::HashMap;
use bevy_rapier3d::control::{KinematicCharacterController, KinematicCharacterControllerOutput};
use bevy_rapier3d::dynamics::RigidBody;
use bevy_rapier3d::geometry::Collider;
use bevy_rapier3d::math::Vect;
use bevy_rapier3d::plugin::{RapierConfiguration, RapierPhysicsPlugin, TimestepMode};
use colored::Colorize;

use crate::util::IntoFnPlugin;

#[linkme::distributed_slice]
pub static TESTS: [fn(&mut App) -> &'static str];

pub fn app() -> App {
	let mut app = App::new();

	#[cfg(feature = "vis_test")]
	app.add_plugins(DefaultPlugins)
		.add_systems(Update, bevy::window::close_on_esc)
		.insert_resource(bevy::winit::WinitSettings {
			return_from_run: true,
			..default()
		})
		.add_plugins(bevy_rapier3d::render::RapierDebugRenderPlugin {
			enabled: true,
			..default()
		});

	#[cfg(not(feature = "vis_test"))]
	app.add_plugins(MinimalPlugins)
		.add_plugins(bevy::log::LogPlugin::default());

	app.add_event::<TestEvent>()
		.init_resource::<RunningTests>()
		.insert_resource(RapierConfiguration {
			gravity: Vect::NEG_Z * 9.81,
			timestep_mode: TimestepMode::Fixed {
				dt: 1.0 / 32.0,
				substeps: 1,
			},
			..default()
		})
		.add_plugins(RapierPhysicsPlugin::<()>::default())
		.insert_resource(Timeout(Timer::from_seconds(600.0, TimerMode::Once)))
		.add_systems(Last, (timeout, check_test_results));

	app
}

#[derive(Resource, Deref, DerefMut)]
pub struct Timeout(pub Timer);

pub fn timeout(mut timer: ResMut<Timeout>, t: Res<Time>) {
	if timer.tick(t.delta()).finished() {
		panic!("Timed out")
	}
}

pub fn exit_app(mut events: EventWriter<AppExit>) {
	events.send(AppExit)
}

#[derive(Event)]
pub struct TestEvent {
	pub name: &'static str,
	pub status: TestStatus,
}

#[derive(Default)]
pub enum TestStatus {
	#[default]
	Running,
	Passed,
	Failed(Box<dyn Error + Send + Sync + 'static>),
}

impl TestStatus {
	fn is_running(&self) -> bool {
		matches!(self, TestStatus::Running)
	}
	fn failed(&self) -> bool {
		matches!(self, TestStatus::Failed(_))
	}
}

impl Display for TestStatus {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			TestStatus::Running => write!(f, "{}", "Running".yellow()),
			TestStatus::Passed => write!(f, "{}", "Passed".green()),
			TestStatus::Failed(err) => write!(f, "{}: {}", "Error".red(), format!("{err}").red()),
		}
	}
}

impl Display for TestEvent {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		let Self { name, status } = self;
		write!(f, "{} {}: {status}", "TEST".blue(), name.bright_black())
	}
}

#[derive(Resource, Default, Deref, DerefMut)]
pub struct RunningTests(HashMap<&'static str, TestStatus>);

fn check_test_results(
	mut results: ResMut<Events<TestEvent>>,
	mut running: ResMut<RunningTests>,
	mut exits: EventWriter<AppExit>,
) {
	for event in results.drain() {
		let message = format!("{event}");
		if event.status.failed() {
			error!("{message}");
		} else {
			info!("{message}");
		}
		running.insert(event.name, event.status);
	}
	let mut failed = 0;
	for status in running.values() {
		if status.is_running() {
			return;
		} else if status.failed() {
			failed += 1;
		}
	}
	if failed > 0 {
		if failed > 1 {
			panic!("{failed} tests failed");
		} else {
			panic!("1 test failed");
		}
	}
	exits.send(AppExit)
}

#[linkme::distributed_slice(TESTS)]
pub fn _app_started(app: &mut App) -> &'static str {
	fn check_app_started(mut events: EventWriter<TestEvent>) {
		events.send(TestEvent {
			name: "app_started",
			status: TestStatus::Passed,
		});
	}

	app.add_systems(Startup, check_app_started);
	"app_started"
}

#[cfg_attr(not(feature = "render"), test)]
fn app_started() {
	let mut app = app();
	_app_started(&mut app);
	app.run();
}
