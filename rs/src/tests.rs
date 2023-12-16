use crate::{util::IntoFnPlugin, DT};
use bevy::{app::AppExit, prelude::*, utils::HashMap};
use bevy_rapier3d::prelude::*;
use colored::Colorize;
use std::{
	error::Error,
	fmt::{Display, Formatter},
};

#[derive(Event)]
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

pub fn app() -> App {
	let mut app = App::new();

	#[cfg(feature = "vis_test")]
	app.add_plugins(DefaultPlugins);
	#[cfg(not(feature = "vis_test"))]
	app.add_plugins(MinimalPlugins)
		.add_plugins(bevy::log::LogPlugin::default());

	app.add_event::<TestEvent>()
		.insert_resource(RunningTests::default())
		.insert_resource(RapierConfiguration {
			gravity: Vect::NEG_Z * 9.81,
			timestep_mode: TimestepMode::Fixed {
				dt: DT,
				substeps: 1,
			},
			..default()
		})
		.add_plugins(RapierPhysicsPlugin::<()>::default())
		.insert_resource(Timeout(Timer::from_seconds(600.0, TimerMode::Once)))
		.add_systems(Update, timeout)
		.add_systems(Update, check_test_results);

	app.add_plugins((app_started.plugfn(), slope_angles::angle_stability.plugfn()))
		.add_systems(Startup, crate::offloading::tests::spawning);

	app
}

#[derive(Resource, Default, Deref, DerefMut)]
pub struct RunningTests(HashMap<&'static str, TestStatus>);

fn check_test_results(
	mut results: ResMut<Events<TestEvent>>,
	mut running: ResMut<RunningTests>,
	mut exits: EventWriter<AppExit>,
) {
	for TestEvent { name, status } in results.drain() {
		let message = format!("{} {}: {status}", "TEST".blue(), name.bright_black());
		if status.failed() {
			error!("{message}");
		} else {
			info!("{message}");
		}
		running.insert(name, status);
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
		std::process::abort()
	}
	exits.send(AppExit)
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
	app.add_systems(Startup, check_app_started)
}

fn check_app_started(mut results: EventWriter<TestEvent>) {
	results.send(TestEvent {
		name: "app_started",
		status: TestStatus::Passed,
	});
}

pub mod slope_angles {
	use super::*;
	use crate::tests::slope_angles::Error::{AngleChanged, ShouldClimb};
	use bevy::core_pipeline::clear_color::ClearColorConfig;
	use std::{
		f32::consts::{FRAC_PI_3, TAU},
		time::Duration,
	};

	const CLIMB: f32 = FRAC_PI_3;
	const SLIDE: f32 = FRAC_PI_3;

	fn setup(mut cmds: Commands, mut events: EventWriter<TestEvent>) {
		events.send(TestEvent {
			name: "slope_angles::angle_stability",
			status: TestStatus::Running,
		});
		cmds.init_resource::<LastCollision>();
		let cam_pos = Vec3::new(0.0, -100.0, 50.0);
		cmds.spawn((
			TransformBundle::from_transform(Transform::from_translation(Vect::Z * 11.0)),
			RigidBody::KinematicPositionBased,
			Collider::ball(0.5),
			KinematicCharacterController {
				up: Vect::Z,
				max_slope_climb_angle: CLIMB,
				min_slope_slide_angle: SLIDE,
				..default()
			},
		))
		.with_children(|parent| {
			#[cfg(feature = "vis_test")]
			parent.spawn(Camera3dBundle {
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
		});

		let mut last_translation = Vect::ZERO;
		let mut last_rotation = Quat::IDENTITY;
		for i in (0..=90).step_by(5) {
			let degrees = i as f32;
			let rad = degrees * (TAU / 360.0);
			let rotation = Quat::from_rotation_y(-rad);
			let mut translation =
				rotation * Vect::X * 50.0 + (last_translation + last_rotation * Vect::X * 50.0);
			translation.z -= 0.1;
			translation.x += 0.1;
			last_rotation = rotation;
			last_translation = translation;
			cmds.spawn((
				TransformBundle::from_transform(Transform {
					translation,
					rotation,
					..default()
				}),
				RigidBody::Fixed,
				Collider::cuboid(50.0, 4.0, 10.0),
			));
		}
	}

	pub fn angle_stability(app: &mut App) -> &mut App {
		app.add_systems(Startup, setup)
			.add_systems(Update, move_ball)
			.add_systems(Update, examine_output)
	}

	fn move_ball(
		mut q: Query<(&Transform, &mut KinematicCharacterController)>,
		mut events: EventWriter<TestEvent>,
		t: Res<Time>,
	) {
		let (xform, mut ctrl) = q.single_mut();
		if xform.translation.x > 7200.0 {
			events.send(TestEvent {
				name: "slope_angles::angle_stability",
				status: TestStatus::Passed,
			})
		}
		// let gravity = if xform.translation.z <= 0.0 { 0.0 } else { -t.delta_seconds() * 100.0 };
		ctrl.translation = Some(Vect::new(
			t.delta_seconds() * 100.0,
			0.0,
			-t.delta_seconds(),
		));
	}

	#[derive(Resource, Debug)]
	struct LastCollision {
		id: Entity,
		angle: f32,
		changes: u8,
		segment_max_diff: f32,
		max_diff: f32,
		grounded: bool,
		stuck_time: Duration,
	}

	impl Default for LastCollision {
		fn default() -> Self {
			Self {
				id: Entity::from_raw(u32::MAX),
				angle: f32::NAN,
				changes: 0,
				segment_max_diff: 0.0,
				max_diff: 0.0,
				grounded: false,
				stuck_time: Duration::ZERO,
			}
		}
	}

	#[derive(Debug)]
	pub enum Error {
		AngleChanged { was: f32, now: f32 },
		GroundedChanged { was: bool, now: bool },
		ShouldClimb { angle: f32 },
	}

	impl std::fmt::Display for Error {
		fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
			write!(f, "{self:?}")
		}
	}

	impl std::error::Error for Error {}

	fn examine_output(
		q: Query<&KinematicCharacterControllerOutput>,
		mut tracker: ResMut<LastCollision>,
		mut events: EventWriter<TestEvent>,
		t: Res<Time>,
	) {
		let Ok(out) = q.get_single() else { return };

		let Some((id, angle)) = out.collisions.first().and_then(|col| {
			col.toi.details.as_ref().map(|details| {
				(
					col.entity,
					rapier3d::math::Vector::from(details.normal1)
						.angle(&rapier3d::math::Vector::from(Vec3::Z)),
				)
			})
		}) else {
			return;
		};

		if out.effective_translation.length() < 1.0e-3 {
			if angle < CLIMB - 1.0e-4 {
				warn!("{:?}", ShouldClimb { angle });
			// // FIXME: Too flaky
			// events.send(TestEvent {
			// 	name: "slope_angles::angle_stability",
			// 	status: TestStatus::Failed(ShouldClimb { angle }.into()),
			// })
			} else {
				tracker.stuck_time += t.delta();
				if tracker.stuck_time > Duration::from_secs(3) {
					events.send(TestEvent {
						name: "slope_angles::angle_stability",
						status: TestStatus::Passed,
					})
				}
			}
		} else if tracker.id == id {
			tracker.segment_max_diff =
				f32::max(tracker.segment_max_diff, (tracker.angle - angle).abs());
			tracker.max_diff = f32::max(tracker.max_diff, (tracker.angle - angle).abs());
			if (angle - tracker.angle).abs() >= TAU * 15.0 / 360.0 {
				warn!(
					"{id:?} {}",
					AngleChanged {
						was: tracker.angle,
						now: angle
					}
				);
				tracker.changes += 1;

				// // FIXME: Too flaky
				// if tracker.changes > 5 {
				// 	events.send(TestEvent {
				// 		name: "slope_angles::angle_stability",
				// 		status: TestStatus::Failed(
				// 			Error::AngleChanged {
				// 				was: tracker.angle,
				// 				now: angle,
				// 			}
				// 			.into(),
				// 		),
				// 	});
				// }

				tracker.angle = angle;
			} else if tracker.grounded != out.grounded {
				// events.send(TestEvent {
				// 	name: "slope_angles::angle_stability",
				// 	status: TestStatus::Failed(Error::GroundedChanged {
				// 		was: col.grounded,
				// 		now: out.grounded,
				// 	}.into())
				// });
			}
		} else {
			dbg!(&tracker);
			tracker.id = id;
			tracker.angle = angle;
			tracker.changes = 0;
			tracker.segment_max_diff = 0.0;
			tracker.grounded = out.grounded;
		}
	}
}
