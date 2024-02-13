use std::{
	error::Error,
	fmt::{Display, Formatter},
};

use bevy::{
	app::{App, AppExit},
	log::{error, info},
	prelude::*,
	utils::HashMap,
};
use bevy_rapier3d::{
	math::Vect,
	plugin::{RapierConfiguration, RapierPhysicsPlugin, TimestepMode},
};
use colored::Colorize;
pub use linkme::distributed_slice as __static_register_test;

#[__static_register_test]
pub static TESTS: [fn(&mut App) -> &'static str] = [..];

pub fn new_test_app() -> App {
	let mut app = App::new();

	#[cfg(feature = "vis_test")]
	app.add_plugins(DefaultPlugins)
		.add_systems(bevy::app::Update, bevy::window::close_on_esc)
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
	pub fn is_running(&self) -> bool {
		matches!(self, TestStatus::Running)
	}
	pub fn passed(&self) -> bool {
		matches!(self, TestStatus::Passed)
	}
	pub fn failed(&self) -> bool {
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
		let name = event.name;
		let prev = running.entry(name).or_insert_with(|| {
			// Only print `Running...` the first time
			info!(
				"{}",
				TestEvent {
					name,
					status: TestStatus::Running
				}
			);
			TestStatus::Running
		});

		if event.status.failed() {
			error!("{event}");
		} else if event.status.passed() {
			info!("{event}");
		}

		*prev = event.status;
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

pub trait TestSystem<M>: IntoSystem<(), TestStatus, M> {
	fn test(self, test_name: &'static str) -> impl System<In = (), Out = ()>;
}

impl<F: IntoSystem<(), TestStatus, M>, M> TestSystem<M> for F {
	fn test(self, test_name: &'static str) -> impl System<In = (), Out = ()> {
		let status_to_event = move |In(status): In<TestStatus>,
		                            mut events: EventWriter<TestEvent>| {
			events.send(TestEvent {
				status,
				name: test_name,
			});
		};

		self.pipe(IntoSystem::into_system(status_to_event))
	}
}

/// A test that runs in the Bevy app.
///
/// Graphical tests cannot use the normal Rust test harness because
///   1) It runs tests off of the main thread, and Bevy's rendering backends can only be
///       started on the main thread.
///   2) The Winit EventLoop cannot be instantiated more than once per process, so separate
///       tests cannot have their own app.
///
/// This macro handles registering test plugins to be run with the `visual` integration test,
/// as well as regular `#[test]` functions when the `render` feature is not enabled.
///
/// ### Usage:
/// ```no_run
/// # use bevy::prelude::*;
/// # use sond_has_engine::{bevy_test, testing::*};
/// // Parameter names can be any non-reserved identifier.
/// bevy_test!(fn test_plugin(app, test_id) {
///     fn check_app_started() -> TestStatus {
///   	    TestStatus::Passed
///     }
///     // See `sond_has_engine::testing::TestSystem`
///     app.add_systems(Startup, check_app_started.test(test_id));
/// });
/// ```
/// The trait [TestSystem] is provided for conveniently piping systems that return [TestStatus]
/// into a systems that send [TestEvent]s which are how the custom harness tracks test progress.
///
/// Optional type annotations may be included:
/// ```no_run
/// # use bevy::prelude::*;
/// # use sond_has_engine::{bevy_test, testing::*};
/// bevy_test!(fn annotated(app: &mut App, test_id: &'static str) { });
/// ```
/// The identifier for `test_id` will be part of a `let` binding, so it cannot be `_`, even
/// if you do not use it. It will be set to the `stringify`'d function name provided. Thus
/// the `test_plugin` above could be written with this instead:
/// ```ignore
/// app.add_systems(Startup, check_app_started.test("test_plugin"));
/// ```
/// But that would require keeping the function name and string in-sync for proper event tracking.
#[macro_export]
macro_rules! bevy_test {
	(fn $name:ident ( $app:ident $(: &mut App)? , $test_id:ident $(: &'static str)? ) $body:block) => {
		// TODO: `linkme` does not work on wasm. Find a usable WASM testing method.
		#[cfg(all(feature = "render", feature = "testing", not(target_arch = "wasm32")))]
		#[allow(unused)]
		#[$crate::testing::__static_register_test($crate::testing::TESTS)]
		fn $name($app: &mut ::bevy::prelude::App) -> &'static str {
			let $test_id = stringify!($name);
			$body
			$test_id
		}

		#[cfg(all(not(feature = "render"), feature = "testing"))]
		#[test]
		fn $name() {
			let mut $app = $crate::testing::new_test_app();
			let $test_id = stringify!($name);
			$body
			$app.run()
		}
	};
}

bevy_test!(fn app_started(app, test_id) {
	fn check_app_started() -> TestStatus {
		TestStatus::Passed
	}

	app.add_systems(bevy::prelude::Startup, check_app_started.test(test_id));
});
