use bevy::{
	app::AppExit,
	log::{error, info},
	prelude::*,
	utils::HashMap,
};
use bevy_rapier3d::{
	math::Vect,
	plugin::{RapierConfiguration, RapierPhysicsPlugin, TimestepMode},
};
use colored::Colorize;
use std::{
	error::Error,
	fmt::{Display, Formatter},
	num::NonZero,
};

pub fn new_test_app() -> App {
	let mut app = App::new();

	#[cfg(all(feature = "render", not(target_arch = "wasm32")))]
	{
		// Feature is enabled by "testing" but is only needed without "vis_test"
		// This silences "unused_crate_dependencies"
		#[allow(unused_imports, clippy::single_component_path_imports)]
		use bevy_mesh;

		app.add_plugins(DefaultPlugins.set(WindowPlugin {
			exit_condition: bevy::window::ExitCondition::DontExit,
			close_when_requested: false,
			..default()
		}))
		.add_plugins(bevy_inspector_egui::bevy_egui::EguiPlugin)
		.insert_resource(bevy::winit::WinitSettings { ..default() })
		.add_plugins(bevy_rapier3d::render::RapierDebugRenderPlugin {
			enabled: true,
			..default()
		})
		.add_systems(Startup, vis_setup)
		.add_systems(Update, pass_fail_keys);
	}

	#[cfg(any(not(feature = "render"), target_arch = "wasm32"))]
	{
		app.add_plugins(
			DefaultPlugins
				.set(bevy::asset::AssetPlugin {
					mode: AssetMode::Processed,
					..default()
				})
				.disable::<bevy::render::RenderPlugin>()
				.disable::<bevy::core_pipeline::CorePipelinePlugin>()
				.disable::<bevy::sprite::SpritePlugin>()
				.disable::<bevy::ui::UiPlugin>()
				.disable::<bevy::pbr::PbrPlugin>()
				.disable::<bevy::gizmos::GizmoPlugin>()
				.disable::<bevy::winit::WinitPlugin>()
				.disable::<bevy::picking::PickingPlugin>()
				.disable::<bevy::picking::input::PointerInputPlugin>()
				.disable::<bevy::picking::InteractionPlugin>()
				.disable::<bevy::text::TextPlugin>(),
		)
		.init_asset::<bevy_mesh::Mesh>();
	}

	app.add_event::<TestEvent>()
		.init_resource::<RunningTests>()
		.insert_resource(TimestepMode::Interpolated {
			dt: crate::DT,
			time_scale: 1.0,
			substeps: 1,
		})
		.add_plugins(RapierPhysicsPlugin::<()>::default())
		.insert_resource(Timeout(Timer::from_seconds(600.0, TimerMode::Once)))
		.add_systems(Last, (timeout, check_test_results));

	fn setup(mut cfg: Single<&mut RapierConfiguration>) {
		cfg.gravity = Vect::NEG_Z * 9.81;
	}

	app.add_systems(Startup, setup);

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
	events.send(AppExit::Success);
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
	pub fn event(self, test_name: &'static str) -> TestEvent {
		TestEvent {
			name: test_name,
			status: self,
		}
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
			info!(
				"{}",
				TestEvent {
					name,
					status: TestStatus::Running
				}
			);
			TestStatus::Running
		});

		if event.status.failed() && !prev.failed() {
			error!("{event}");
		} else if event.status.passed() && !prev.passed() {
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
			error!("{failed} tests failed");
		} else {
			error!("1 test failed");
		}
		exits.send(AppExit::Error(
			failed.try_into().unwrap_or(NonZero::<u8>::MAX),
		));
	} else {
		exits.send(AppExit::Success);
	}
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

		IntoSystem::into_system(self.pipe(IntoSystem::into_system(status_to_event)))
	}
}

/// A test that runs in the Bevy app.
///
/// Graphical tests cannot use the normal Rust test harness because
///   1) It runs tests off of the main thread, and Bevy's rendering backends can only be
///      started on the main thread.
///   2) The Winit EventLoop cannot be instantiated more than once per process, so separate
///      tests cannot have their own app.
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
///         TestStatus::Passed
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
/// The identifier for `test_id` will be part of a `let` binding and returned, so it cannot be `_`,
/// even if you do not use it. It will be set to the `stringify`'d function name provided. Thus
/// the `test_plugin` above could be written with this instead:
/// ```ignore
/// app.add_systems(Startup, check_app_started.test("test_plugin"));
/// ```
/// But that would require keeping the function name and string in-sync for proper event tracking.
#[macro_export]
macro_rules! bevy_test {
	(fn $name:ident ( $app:ident $(: &mut App)? , $test_id:ident $(: &'static str)? ) $body:block) => {
		// TODO: `linkme` does not work on wasm. Find a usable WASM testing method.
		#[cfg(all(feature = "render", not(target_arch = "wasm32")))]
		#[allow(unused)]
		#[$crate::testing::__static_register_test($crate::testing::TESTS)]
		fn $name($app: &mut ::bevy::prelude::App) -> &'static str {
			#[allow(unused_imports)]
			use $crate::testing::TestSystem as _;
			let $test_id = stringify!($name);
			$body
			$test_id
		}

		#[cfg(any(not(feature = "render"), target_arch = "wasm32"))]
		#[test]
		fn $name() -> ::bevy::prelude::AppExit {
			#[allow(unused_imports)]
			use $crate::testing::TestSystem as _;
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

#[cfg(all(feature = "render", not(target_arch = "wasm32")))]
pub mod vis {
	use crate::testing::{TestEvent, TestStatus};
	use bevy::app::App;
	use bevy::input::ButtonInput;
	use bevy::prelude::*;

	pub use linkme::distributed_slice as __static_register_test;

	#[__static_register_test]
	pub static TESTS: [fn(&mut App) -> &'static str] = [..];

	#[derive(Component, Debug)]
	#[require(Window)]
	pub struct TestWindow {
		pub test_id: &'static str,
	}

	impl TestWindow {
		pub fn new(test_id: &'static str) -> Self {
			Self { test_id }
		}
	}

	pub fn vis_setup(mut cmds: Commands) {
		cmds.spawn(Camera2d);
		cmds.spawn(Text::new("Running tests..."));
	}

	pub fn pass_fail_keys(
		mut commands: Commands,
		focused_windows: Query<(Entity, &Window, &TestWindow)>,
		input: Res<ButtonInput<KeyCode>>,
		mut events: EventWriter<TestEvent>,
	) {
		for (entity, window, test_window) in focused_windows.iter() {
			if !window.focused {
				continue;
			}

			if input.just_pressed(KeyCode::KeyF) {
				commands.entity(entity).despawn();
				events.send(TestStatus::Failed("user pressed F".into()).event(test_window.test_id));
			} else if input.just_pressed(KeyCode::KeyP) {
				commands.entity(entity).despawn();
				events.send(TestStatus::Passed.event(test_window.test_id));
			} else {
				events.send(TestStatus::Running.event(test_window.test_id));
			}
		}
	}
}
#[cfg(all(feature = "render", not(target_arch = "wasm32")))]
pub use vis::*;
