//! Regression tests pinning the behavior of core game systems.
//!
//! Everything here is headless and deterministic: a bare `App` driven by
//! `TimePlugin` and `TimeUpdateStrategy::ManualDuration`, so each
//! `App::update` advances `Time` by exactly `engine::DT`. The same assertions
//! must keep passing after the Bevy 0.16 -> 0.17 migration.

use super::*;
use bevy::time::{TimePlugin, TimeUpdateStrategy};
use enum_components::EntityWorldEnumMut;
use planet::PlanetVec2;
use player::{countdown_respawn, PlayerId, PlayerRespawnTimers, PlayerSpawnEvent};
use std::time::Duration;

/// The fixed timestep the game runs at.
fn dt() -> Duration {
	Duration::from_secs_f32(engine::DT)
}

/// A minimal headless app whose `Time` advances by exactly one step per
/// `App::update`.
fn test_app() -> App {
	let mut app = App::new();
	app.add_plugins(TimePlugin)
		.insert_resource(TimeUpdateStrategy::ManualDuration(dt()));
	// The very first update only initializes the clock (`Time::delta()` is
	// still zero), so burn it before any system under test gets scheduled.
	app.update();
	app
}

/// A `PlanetVec2` at the given planet coordinates.
fn planet_at(x: f64, y: f64) -> PlanetVec2 {
	let mut point = PlanetVec2::default();
	point.x = x;
	point.y = y;
	point
}

/// A centered frame with a `min_width` of 10 that triggers at `bounds`.
fn frame_at(bounds: f32) -> Frame {
	Frame {
		center: PlanetVec2::default(),
		trigger_bounds: bounds,
		min_width: 10.0,
	}
}

/// Spawns a player root entity at the given translation.
fn spawn_root(app: &mut App, x: f32, y: f32) {
	app.world_mut()
		.spawn(GlobalTransform::from_xyz(x, y, 0.0))
		.set_enum(Root);
}

#[test]
fn shift_frame_grows_for_spread_players() {
	let mut app = test_app();
	app.insert_resource(frame_at(100.0))
		.insert_resource(Prev::<Frame>::default());
	spawn_root(&mut app, -60.0, 0.0);
	spawn_root(&mut app, 60.0, 0.0);
	app.add_systems(Update, shift_frame);
	app.update();

	let frame = app.world().resource::<Frame>();
	let prev = app.world().resource::<Prev<Frame>>();
	// Players 120 apart exceed the 100-wide frame, so it grows by 1.5x.
	assert_eq!(frame.trigger_bounds, 180.0);
	// Their midpoint is centered, so the frame doesn't move.
	assert_eq!(frame.center, planet_at(0.0, 0.0));
	// `Prev<Frame>` snapshots the pre-shift state for `reframe_all_entities`.
	assert_eq!(prev.trigger_bounds, 100.0);
}

#[test]
fn shift_frame_shrinks_and_follows_lone_player() {
	let mut app = test_app();
	app.insert_resource(frame_at(100.0))
		.insert_resource(Prev::<Frame>::default());
	spawn_root(&mut app, 50.0, 0.0);
	app.add_systems(Update, shift_frame);
	app.update();

	let frame = app.world().resource::<Frame>();
	// A lone player collapses the frame back to `min_width`...
	assert_eq!(frame.trigger_bounds, 10.0);
	// ...then the frame steps a full width toward the player.
	assert_eq!(frame.center, planet_at(10.0, 0.0));
	let prev = app.world().resource::<Prev<Frame>>();
	assert_eq!(prev.center, planet_at(0.0, 0.0));
}

#[test]
fn terminal_velocity_drags_only_axes_past_their_limit() {
	let mut app = test_app();
	let entity = app
		.world_mut()
		.spawn((
			CtrlVel {
				linvel: Vect::new(10.0, -10.0, 5.0),
				angvel: Vect::new(4.0, -6.0, 0.0),
			},
			TerminalVelocity(Velocity {
				linvel: Vect::new(5.0, 5.0, 5.0),
				angvel: Vect::new(5.0, 5.0, 5.0),
			}),
		))
		.id();
	app.add_systems(Update, terminal_velocity);
	app.update();

	let CtrlVel { linvel, angvel } = *app.world().get::<CtrlVel>(entity).unwrap();
	let step = engine::DT;
	// Only axes strictly beyond their limit are dragged back one step...
	assert_eq!(linvel.x, 10.0 - step);
	assert_eq!(linvel.y, -(10.0 - step));
	assert_eq!(linvel.z, 5.0); // exactly at the limit: untouched
	assert_eq!(angvel.x, 4.0); // within the limit: untouched
	assert_eq!(angvel.y, -(6.0 - step));
	assert_eq!(angvel.z, 0.0);
	// ...and the drag continues every frame.
	app.update();
	let CtrlVel { linvel, .. } = *app.world().get::<CtrlVel>(entity).unwrap();
	assert_eq!(linvel.x, 10.0 - 2.0 * step);
}

#[test]
fn despawn_oob_despawns_or_resets() {
	let mut app = test_app();
	app.insert_resource(AbsoluteBounds { extents: 100.0 });
	let oob = app
		.world_mut()
		.spawn(GlobalTransform::from_xyz(1000.0, 0.0, 0.0))
		.id();
	let on_boundary = app
		.world_mut()
		.spawn(GlobalTransform::from_xyz(100.0, 0.0, 0.0))
		.id();
	let pinned = app
		.world_mut()
		.spawn((GlobalTransform::from_xyz(0.0, 0.0, -1000.0), NeverDespawn))
		.id();
	app.add_systems(PostUpdate, despawn_oob);
	app.update();

	// Entities beyond the bounds are despawned...
	assert!(app.world().get_entity(oob).is_err());
	// ...but entities exactly on the boundary are in bounds.
	assert!(app.world().get_entity(on_boundary).is_ok());
	// `NeverDespawn` entities are teleported to the origin instead.
	assert_eq!(
		app.world().get::<GlobalTransform>(pinned).unwrap(),
		&GlobalTransform::default()
	);
}

#[derive(Component, Debug, PartialEq)]
struct TestAbility;

impl Ability for TestAbility {
	fn cooldown(&mut self) -> Duration {
		Duration::from_millis(500)
	}
}

#[test]
fn tick_cooldown_expires_and_reinserts_ability() {
	let mut app = test_app();
	let entity = app
		.world_mut()
		.spawn(Cooldown::<TestAbility>::new(TestAbility))
		.id();
	app.add_systems(Update, tick_cooldown::<TestAbility>);

	// Each frame counts down exactly one step.
	app.update();
	assert_eq!(
		app.world().get::<Cooldown<TestAbility>>(entity).unwrap().t,
		Duration::from_millis(500) - dt()
	);
	assert!(app.world().get::<TestAbility>(entity).is_none());

	// Reaching exactly zero keeps the cooldown for that frame...
	let ticks = (Duration::from_millis(500).as_nanos() / dt().as_nanos()) as u32;
	for _ in 1..ticks {
		app.update();
	}
	assert_eq!(
		app.world().get::<Cooldown<TestAbility>>(entity).unwrap().t,
		Duration::ZERO
	);
	assert!(app.world().get::<TestAbility>(entity).is_none());

	// ...and the next frame removes it, re-inserting the ability.
	app.update();
	assert!(app.world().get::<Cooldown<TestAbility>>(entity).is_none());
	assert_eq!(app.world().get::<TestAbility>(entity), Some(&TestAbility));
}

/// Collects every `PlayerSpawnEvent` so tests can assert on them after any
/// number of frames.
#[derive(Resource, Default, Deref, DerefMut)]
struct CollectedSpawns(Vec<PlayerSpawnEvent>);

fn collect_spawns(
	mut events: EventReader<PlayerSpawnEvent>,
	mut collected: ResMut<CollectedSpawns>,
) {
	collected.extend(events.read().cloned());
}

#[test]
fn countdown_respawn_emits_spawn_event_when_timer_expires() {
	let mut app = test_app();
	app.add_event::<PlayerSpawnEvent>()
		.insert_resource(PlayerRespawnTimers::default())
		.init_resource::<CollectedSpawns>()
		.add_systems(
			Last,
			(countdown_respawn, collect_spawns.after(countdown_respawn)),
		);

	let player = PlayerId::new(1).unwrap();
	let died_at = planet_at(12.5, -7.0);
	// Starting a fresh timer reports the remaining duration.
	assert_eq!(
		app.world_mut().resource_mut::<PlayerRespawnTimers>().start(
			player,
			died_at,
			Duration::from_secs(2)
		),
		Err(Duration::from_secs(2))
	);

	// One frame short of the 2s respawn: no event yet.
	let ticks = (Duration::from_secs(2).as_nanos() / dt().as_nanos()) as u32;
	for _ in 1..ticks {
		app.update();
	}
	assert!(app.world().resource::<CollectedSpawns>().is_empty());

	// The frame the timer completes emits exactly one spawn event...
	app.update();
	let collected = &app.world().resource::<CollectedSpawns>().0;
	assert_eq!(collected.len(), 1);
	assert_eq!(collected[0].id, player);
	assert_eq!(collected[0].died_at, died_at);

	// ...and a `TimerMode::Once` timer never re-fires.
	app.update();
	app.update();
	assert_eq!(app.world().resource::<CollectedSpawns>().len(), 1);
}
