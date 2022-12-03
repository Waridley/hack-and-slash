use crate::util::FnPluginExt;
use bevy::utils::HashMap;
use bevy::{app::AppExit, prelude::*};
use bevy_rapier3d::prelude::*;
use colored::Colorize;
use std::fmt::Formatter;
use std::{error::Error, fmt::Display};

pub struct TestEvent {
	name: &'static str,
	status: TestStatus,
}

pub enum TestStatus {
	Running,
	Passed,
	Failed(Box<dyn Error + Send + Sync + 'static>),
}

impl TestStatus {
	fn is_running(&self) -> bool {
		matches!(self, TestStatus::Running)
	}
}

impl Display for TestStatus {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			TestStatus::Running => write!(f, "{}", "Running".yellow()),
			TestStatus::Passed => write!(f, "{}", "Passed".green()),
			TestStatus::Failed(err) => write!(f, "Error: {}", format!("{err}").red()),
		}
	}
}

pub fn app() -> App {
	let mut app = App::new();

	#[cfg(feature = "vis_test")]
	app.add_plugins(DefaultPlugins);
	#[cfg(not(feature = "vis_test"))]
	app.add_plugins(MinimalPlugins)
		.add_plugin(bevy::log::LogPlugin::default());

	app.add_event::<TestEvent>()
		.insert_resource(RunningTests::default())
		.add_plugin(RapierPhysicsPlugin::<()>::default())
		.insert_resource(Timeout(Timer::from_seconds(60.0, TimerMode::Once)))
		.add_system(timeout)
		.add_system(check_test_results);

	app.fn_plugin(app_started)
		.fn_plugin(slope_angles::angle_stability);

	app
}

#[derive(Resource, Default, Deref, DerefMut)]
pub struct RunningTests(HashMap<&'static str, TestStatus>);

fn check_test_results(
	mut results: ResMut<Events<TestEvent>>,
	mut running: ResMut<RunningTests>,
	mut exit: EventWriter<AppExit>,
) {
	use TestStatus::*;
	for TestEvent { name, status } in results.drain() {
		info!("{} {name}: {status}", "TEST".blue());
		match status {
			Running => running.insert(name, Running),
			Passed => running.insert(name, Passed),
			Failed(err) => running.insert(name, Failed(err)),
		};
	}
	for status in running.values() {
		if status.is_running() {
			return;
		}
	}
	exit.send(AppExit)
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

pub fn app_started(app: &mut App) -> &mut App {
	app.add_startup_system(check_app_started)
}

fn check_app_started(mut results: EventWriter<TestEvent>) {
	results.send(TestEvent {
		name: "app_started",
		status: TestStatus::Passed,
	});
}

pub mod slope_angles {
	use bevy::core_pipeline::clear_color::ClearColorConfig;
	use bevy::prelude::*;
	use bevy_rapier3d::prelude::*;

	fn setup(mut cmds: Commands) {
		let cam_pos = Vec3::new(0.0, -10.0, 2.0);
		cmds.spawn(Camera3dBundle {
			transform: Transform {
				translation: cam_pos,
				rotation: Quat::from_rotation_arc(Vec3::NEG_Z, -cam_pos.normalize()),
				..default()
			},
			camera_3d: Camera3d {
				clear_color: ClearColorConfig::Custom(Color::BLACK),
				..default()
			},
			..default()
		});
		cmds.spawn((RigidBody::KinematicPositionBased, Collider::ball(0.5)));
	}

	pub fn angle_stability(app: &mut App) -> &mut App {
		app.add_startup_system(setup)
	}
}
