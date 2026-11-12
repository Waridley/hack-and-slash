use bevy::{
	app::AppExit,
	log::{error, info},
	prelude::*,
};
use bevy_rapier3d::{
	math::Vect,
	plugin::{RapierConfiguration, RapierPhysicsPlugin, TimestepMode},
};
use colored::Colorize;
use std::{
	collections::HashMap,
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
		.add_plugins(bevy_inspector_egui::bevy_egui::EguiPlugin {
			enable_multipass_for_primary_context: true,
		})
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
	events.write(AppExit::Success);
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
		exits.write(AppExit::Error(
			failed.try_into().unwrap_or(NonZero::<u8>::MAX),
		));
	} else {
		exits.write(AppExit::Success);
	}
}

pub trait TestSystem<M>: IntoSystem<(), TestStatus, M> {
	fn test(self, test_name: &'static str) -> impl System<In = (), Out = ()>;
}

impl<F: IntoSystem<(), TestStatus, M>, M> TestSystem<M> for F {
	fn test(self, test_name: &'static str) -> impl System<In = (), Out = ()> {
		let status_to_event = move |In(status): In<TestStatus>,
		                            mut events: EventWriter<TestEvent>| {
			events.write(TestEvent {
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

/// A visual test that can run either offscreen (automated) or in a window
/// (manual review).
///
/// The setup function receives a [`VisualTestContext`] whose `target` is either
/// an image render target (automated mode) or a window render target (review
/// mode). Returning the test name lets the runner register the test with the
/// result-tracking system.
///
/// Automated tests are registered in `AUTOMATED_VISUAL_TESTS`, and manual tests
/// in `MANUAL_VISUAL_TESTS`. The review runner runs both; the automated runner
/// runs only automated tests.
#[macro_export]
macro_rules! bevy_visual_test {
	(automated fn $name:ident ($app:ident, $ctx:ident) $body:block) => {
		#[cfg(all(feature = "vis_test", not(target_arch = "wasm32")))]
		#[allow(unused)]
		#[$crate::testing::visual::__static_register_visual_test($crate::testing::visual::AUTOMATED_VISUAL_TESTS)]
		fn $name(
			$app: &mut ::bevy::prelude::App,
			$ctx: $crate::testing::visual::VisualTestContext,
		) -> &'static str {
			let test_id = stringify!($name);
			$body
			test_id
		}
	};

	(manual fn $name:ident ($app:ident, $ctx:ident) $body:block) => {
		#[cfg(all(feature = "vis_test", not(target_arch = "wasm32")))]
		#[allow(unused)]
		#[$crate::testing::visual::__static_register_visual_test($crate::testing::visual::MANUAL_VISUAL_TESTS)]
		fn $name(
			$app: &mut ::bevy::prelude::App,
			$ctx: $crate::testing::visual::VisualTestContext,
		) -> &'static str {
			let test_id = stringify!($name);
			$body
			test_id
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
	use bevy::{app::App, input::ButtonInput, prelude::*};

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
				events
					.write(TestStatus::Failed("user pressed F".into()).event(test_window.test_id));
			} else if input.just_pressed(KeyCode::KeyP) {
				commands.entity(entity).despawn();
				events.write(TestStatus::Passed.event(test_window.test_id));
			} else {
				events.write(TestStatus::Running.event(test_window.test_id));
			}
		}
	}
}
#[cfg(all(feature = "vis_test", not(target_arch = "wasm32")))]
pub mod visual {
	use super::{RunningTests, TestEvent, TestStatus, Timeout};
	use bevy::{
		app::{AppExit, ScheduleRunnerPlugin},
		core_pipeline::{tonemapping::Tonemapping, Skybox},
		ecs::observer::Trigger,
		log::{error, info},
		prelude::*,
		render::{
			camera::RenderTarget,
			render_asset::RenderAssetUsages,
			render_resource::{
				Extent3d, TextureDescriptor, TextureDimension, TextureFormat, TextureUsages,
				TextureViewDescriptor, TextureViewDimension,
			},
			view::screenshot::{save_to_disk, Screenshot, ScreenshotCaptured},
		},
		time::TimeUpdateStrategy,
		window::{Window, WindowPlugin, WindowRef},
		winit::WinitPlugin,
	};
	use std::{f32::consts::FRAC_PI_2, num::NonZero, path::PathBuf, time::Duration};

	pub use linkme::distributed_slice as __static_register_visual_test;

	#[derive(Clone, Copy, Debug, Eq, PartialEq)]
	pub enum VisualTestMode {
		Automated,
		Review,
	}

	#[derive(Clone, Debug)]
	pub struct VisualTestContext {
		pub mode: VisualTestMode,
		pub target: RenderTarget,
		pub image: Option<Handle<Image>>,
		pub window: Option<Entity>,
	}

	#[__static_register_visual_test]
	pub static AUTOMATED_VISUAL_TESTS: [fn(&mut App, VisualTestContext) -> &'static str] = [..];

	#[__static_register_visual_test]
	pub static MANUAL_VISUAL_TESTS: [fn(&mut App, VisualTestContext) -> &'static str] = [..];

	const IMAGE_WIDTH: u32 = 640;
	const IMAGE_HEIGHT: u32 = 480;
	const CAPTURE_FRAME: u32 = 10;
	const MAX_CAPTURE_FRAME: u32 = 60;

	#[derive(Resource)]
	struct AutoCapture {
		target: Handle<Image>,
		path: PathBuf,
		test_name: &'static str,
		frame: u32,
		spawned: bool,
	}

	fn new_visual_auto_app() -> App {
		let mut app = App::new();
		app.add_plugins(
			DefaultPlugins
				.set(bevy::asset::AssetPlugin {
					file_path: concat!(env!("CARGO_MANIFEST_DIR"), "/../assets").into(),
					..default()
				})
				.set(WindowPlugin {
					primary_window: None,
					exit_condition: bevy::window::ExitCondition::DontExit,
					close_when_requested: false,
					..default()
				})
				.disable::<WinitPlugin>(),
		)
		.add_plugins(ScheduleRunnerPlugin::run_loop(Duration::from_secs_f64(
			1.0 / 60.0,
		)))
		.insert_resource(TimeUpdateStrategy::ManualDuration(Duration::from_secs_f64(
			1.0 / 60.0,
		)));

		app.add_event::<TestEvent>();
		app
	}

	fn new_visual_review_app() -> App {
		let mut app = App::new();
		app.add_plugins(
			DefaultPlugins
				.set(bevy::asset::AssetPlugin {
					file_path: concat!(env!("CARGO_MANIFEST_DIR"), "/../assets").into(),
					..default()
				})
				.set(WindowPlugin {
					primary_window: Some(Window {
						title: "Visual test review".into(),
						..default()
					}),
					exit_condition: bevy::window::ExitCondition::DontExit,
					close_when_requested: false,
					..default()
				}),
		);

		app.add_event::<TestEvent>()
			.init_resource::<RunningTests>()
			.insert_resource(Timeout(Timer::from_seconds(600.0, TimerMode::Once)))
			.add_systems(Startup, super::vis::vis_setup)
			.add_systems(Last, (super::timeout, super::check_test_results))
			.add_systems(Update, super::vis::pass_fail_keys);

		app
	}

	fn add_auto_target(app: &mut App) -> VisualTestContext {
		let size = Extent3d {
			width: IMAGE_WIDTH,
			height: IMAGE_HEIGHT,
			..default()
		};
		let mut image = Image::new_fill(
			size,
			TextureDimension::D2,
			&[0, 0, 0, 255],
			TextureFormat::Rgba8UnormSrgb,
			RenderAssetUsages::default(),
		);
		image.texture_descriptor.usage |= TextureUsages::COPY_SRC
			| TextureUsages::RENDER_ATTACHMENT
			| TextureUsages::TEXTURE_BINDING;

		let handle = app.world_mut().resource_mut::<Assets<Image>>().add(image);

		VisualTestContext {
			mode: VisualTestMode::Automated,
			target: RenderTarget::Image(handle.clone().into()),
			image: Some(handle),
			window: None,
		}
	}

	fn add_auto_capture(app: &mut App, ctx: &VisualTestContext, name: &'static str) {
		let Some(target) = ctx.image.clone() else {
			return;
		};

		let path = PathBuf::from(format!("visual-tests/{name}.png"));
		if let Some(parent) = path.parent() {
			let _ = std::fs::create_dir_all(parent);
		}

		app.insert_resource(AutoCapture {
			target,
			path,
			test_name: name,
			frame: 0,
			spawned: false,
		})
		.add_systems(Update, spawn_auto_capture);
	}

	fn screenshot_status(image: &Image) -> TestStatus {
		let Some(data) = image.data.as_ref() else {
			return TestStatus::Failed("screenshot has no CPU pixel data".into());
		};
		if data.len() < 4 {
			return TestStatus::Failed("screenshot is too small".into());
		}

		let pixel = &data[0..4];
		if pixel[0] == 0 && pixel[1] == 0 && pixel[2] == 0 {
			return TestStatus::Failed("screenshot is blank".into());
		}

		TestStatus::Passed
	}

	fn spawn_auto_capture(
		mut commands: Commands,
		mut capture: ResMut<AutoCapture>,
		mut exits: EventWriter<AppExit>,
	) {
		capture.frame += 1;

		if capture.spawned {
			return;
		}

		if capture.frame > MAX_CAPTURE_FRAME {
			capture.spawned = true;
			exits.write(AppExit::Error(NonZero::<u8>::new(1).unwrap()));
			return;
		}

		if capture.frame < CAPTURE_FRAME {
			return;
		}

		capture.spawned = true;
		let path = capture.path.clone();
		let target = capture.target.clone();
		let name = capture.test_name;

		commands.spawn(Screenshot::image(target)).observe(
			move |trigger: Trigger<ScreenshotCaptured>,
			      mut events: EventWriter<TestEvent>,
			      mut exits: EventWriter<AppExit>| {
				let image = trigger.event().0.clone();
				let status = screenshot_status(&image);
				let passed = status.passed();
				events.write(status.event(name));
				save_to_disk(&path)(trigger);

				if passed {
					exits.write(AppExit::Success);
				} else {
					exits.write(AppExit::Error(NonZero::<u8>::new(1).unwrap()));
				}
			},
		);
	}

	pub fn run_automated_visual_tests() -> AppExit {
		let mut failures = 0u8;

		for test in AUTOMATED_VISUAL_TESTS.iter().copied() {
			let mut app = new_visual_auto_app();
			let ctx = add_auto_target(&mut app);
			let name = test(&mut app, ctx.clone());
			add_auto_capture(&mut app, &ctx, name);

			match app.run() {
				AppExit::Success => info!("TEST {name}: Passed"),
				AppExit::Error(_) => {
					error!("TEST {name}: Failed");
					failures = failures.saturating_add(1);
				}
			}
		}

		if failures == 0 {
			AppExit::Success
		} else {
			AppExit::Error(NonZero::<u8>::new(failures).unwrap())
		}
	}

	pub fn run_review_visual_tests() -> AppExit {
		let mut app = new_visual_review_app();

		for test in AUTOMATED_VISUAL_TESTS
			.iter()
			.copied()
			.chain(MANUAL_VISUAL_TESTS.iter().copied())
		{
			let window = app
				.world_mut()
				.spawn(Window {
					title: "Visual test".into(),
					..default()
				})
				.id();

			let ctx = VisualTestContext {
				mode: VisualTestMode::Review,
				target: RenderTarget::Window(WindowRef::Entity(window)),
				image: None,
				window: Some(window),
			};

			let name = test(&mut app, ctx);

			let mut entity = app.world_mut().entity_mut(window);
			entity.insert(super::vis::TestWindow::new(name));
			if let Some(mut window) = entity.get_mut::<Window>() {
				window.title = name.into();
			}

			app.world_mut()
				.resource_mut::<RunningTests>()
				.insert(name, TestStatus::Running);
		}

		app.run()
	}

	#[cfg(feature = "vis_test")]
	crate::bevy_visual_test!(automated fn offscreen_clear_color(app, ctx) {
		app.insert_resource(ClearColor(Color::srgb_u8(20, 80, 160)));
		app.add_systems(Startup, move |mut commands: Commands| {
			commands.spawn((
				Camera2d,
				Camera {
					target: ctx.target.clone(),
					..default()
				},
			));
		});
	});

	#[cfg(feature = "vis_test")]
	crate::bevy_visual_test!(automated fn offscreen_3d_cube(app, ctx) {
		app.insert_resource(ClearColor(Color::srgb_u8(18, 24, 32)));
		app.insert_resource(AmbientLight {
			color: Color::WHITE,
			brightness: 300.0,
			..default()
		});
		app.add_systems(
			Startup,
			move |mut commands: Commands,
				  mut meshes: ResMut<Assets<Mesh>>,
				  mut materials: ResMut<Assets<StandardMaterial>>| {
				let cube = meshes.add(Cuboid::new(1.6, 1.6, 1.6));
				let material = materials.add(StandardMaterial {
					base_color: Color::srgb(0.85, 0.25, 0.12),
					metallic: 0.1,
					perceptual_roughness: 0.45,
					..default()
				});

				commands.spawn((
					Mesh3d(cube),
					MeshMaterial3d(material),
					Transform::from_xyz(0.0, 0.0, 0.0),
				));

				commands.spawn((
					DirectionalLight {
						illuminance: 4000.0,
						shadows_enabled: false,
						..default()
					},
					Transform::from_rotation(
						Quat::from_rotation_x(-0.6) * Quat::from_rotation_y(0.8),
					),
				));

				let camera = commands
					.spawn((
						Camera3d::default(),
						Transform::from_xyz(3.2, 2.4, 4.8).looking_at(Vec3::ZERO, Vec3::Y),
					))
					.id();
				commands.entity(camera).insert(Camera {
					target: ctx.target.clone(),
					..default()
				});
			},
		);
	});

	#[cfg(feature = "vis_test")]
	crate::bevy_visual_test!(automated fn offscreen_skybox(app, ctx) {
		app.add_plugins((
			crate::planet::day_night::DayNightPlugin,
			crate::planet::sky::SkyPlugin,
		));
		app.insert_resource(
			crate::planet::day_night::DayNightCycle::from_tod(0.5).with_day_length(20.0),
		);
		app.insert_resource(ClearColor(Color::BLACK));

		app.add_systems(
			Startup,
			move |mut commands: Commands,
				  mut images: ResMut<Assets<Image>>,
				  asset_server: Res<AssetServer>| {
				let size = Extent3d {
					width: 16,
					height: 96,
					depth_or_array_layers: 1,
				};
				let mut tex = Image {
					texture_descriptor: TextureDescriptor {
						label: Some("skybox_texture"),
						size,
						dimension: TextureDimension::D2,
						format: TextureFormat::Rgba32Float,
						mip_level_count: 1,
						sample_count: 1,
						usage: TextureUsages::TEXTURE_BINDING
							| TextureUsages::COPY_DST
							| TextureUsages::RENDER_ATTACHMENT,
						view_formats: &[],
					},
					texture_view_descriptor: Some(TextureViewDescriptor {
						label: Some("skybox_view"),
						dimension: Some(TextureViewDimension::Cube),
						..default()
					}),
					asset_usage: RenderAssetUsages::RENDER_WORLD,
					..default()
				};
				tex.resize(size);
				tex.reinterpret_stacked_2d_as_array(6);

				let sky_texture = images.add(tex);
				let skybox_shader = asset_server.load::<Shader>("shaders/skybox.wgsl");

				commands.spawn((
					Camera3d::default(),
					Camera {
						hdr: true,
						target: ctx.target.clone(),
						..default()
					},
					Transform::from_rotation(Quat::from_rotation_x(FRAC_PI_2)),
					Tonemapping::BlenderFilmic,
					Skybox {
						image: sky_texture,
						brightness: 1_000.0,
						rotation: Quat::from_rotation_x(FRAC_PI_2),
					},
					crate::planet::sky::SkyShader(skybox_shader),
				));
			},
		);
	});

	#[cfg(feature = "vis_test")]
	crate::bevy_visual_test!(manual fn manual_review_color(app, ctx) {
		app.insert_resource(ClearColor(Color::srgb_u8(160, 60, 20)));
		app.add_systems(Startup, move |mut commands: Commands| {
			commands.spawn((
				Camera2d,
				Camera {
					target: ctx.target.clone(),
					..default()
				},
			));
		});
	});
}

#[cfg(all(feature = "render", not(target_arch = "wasm32")))]
pub use vis::*;
#[cfg(all(feature = "vis_test", not(target_arch = "wasm32")))]
pub use visual::*;
