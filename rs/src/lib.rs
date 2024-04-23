#![warn(unused_crate_dependencies)]

use std::{f32::consts::*, fmt::Debug, time::Duration};

use bevy::{
	diagnostic::FrameTimeDiagnosticsPlugin, prelude::*, render::RenderPlugin, window::PrimaryWindow,
};
// #[allow(unused_imports, clippy::single_component_path_imports)]
// #[cfg(all(feature = "dylib", not(target_arch = "wasm32")))]
// use bevy_dylib;
use bevy_kira_audio::AudioPlugin;
use bevy_pkv::PkvStore;
use bevy_rapier3d::{plugin::PhysicsSet::StepSimulation, prelude::*};
use enum_components::WithVariant;
use particles::{ParticlesPlugin, Spewer};

pub use engine::{anim, mats, nav, offloading, planet, settings, util};
use engine::{
	planet::frame::Frame,
	util::{Angle, Prev},
	EnginePlugin,
};
use offloading::OffloadingPlugin;
use planet::sky::SkyPlugin;
use player::{abilities::AbilitiesPlugin, ctrl::CtrlVel};
#[allow(unused_imports, clippy::single_component_path_imports)]
#[cfg(all(feature = "dylib", not(target_arch = "wasm32")))]
use sond_has_engine_dylib;
use util::IntoFnPlugin;

use crate::{mats::MatsPlugin, player::player_entity::Root};

pub mod enemies;
pub mod pickups;
pub mod player;
#[cfg(feature = "testing")]
pub mod tests;
pub mod ui;

/// Epsilon
pub const EPS: f32 = 1.0e-5;
/// Rotational epsilon in radians
pub const R_EPS: f32 = TAU / 360.0; // 1 degree
/// Fixed delta time
pub const DT: f32 = 1.0 / 30.0;
/// Up vector
pub const UP: Vect = Vect::Z;

#[derive(DynamicPlugin)]
#[repr(C)]
struct GameDynPlugin;

impl Plugin for GameDynPlugin {
	fn build(&self, app: &mut App) {
		app.add_plugins((
			(
				bevy::core::TaskPoolPlugin::default(),
				bevy::core::TypeRegistrationPlugin,
				bevy::core::FrameCountPlugin,
				bevy::time::TimePlugin,
				bevy::transform::TransformPlugin,
				bevy::hierarchy::HierarchyPlugin,
				bevy::diagnostic::DiagnosticsPlugin,
				bevy::input::InputPlugin,
				// bevy::window::WindowPlugin::default(), // Need to give it the window from the editor
				bevy::a11y::AccessibilityPlugin,
				bevy::scene::ScenePlugin,
			),
			// bevy::winit::WinitPlugin::default(), // can't init EventLoop more than once
			(
				RenderPlugin::default(),
				ImagePlugin::default(),
				#[cfg(not(target_arch = "wasm32"))]
				bevy::render::pipelined_rendering::PipelinedRenderingPlugin,
				bevy::core_pipeline::CorePipelinePlugin,
				bevy::sprite::SpritePlugin,
				bevy::text::TextPlugin,
				bevy::ui::UiPlugin,
				bevy::pbr::PbrPlugin::default(),
				bevy::gltf::GltfPlugin::default(),
				bevy::audio::AudioPlugin::default(),
				bevy::gilrs::GilrsPlugin,
				bevy::animation::AnimationPlugin,
				bevy::gizmos::GizmoPlugin,
			),
		));
		#[cfg(feature = "debugging")]
		app.configure_schedules(bevy::ecs::schedule::ScheduleBuildSettings {
			ambiguity_detection: bevy::ecs::schedule::LogLevel::Warn,
			..default()
		});
		game_plugin(app);
	}
}

pub fn game_plugin(app: &mut App) -> &mut App {
	app.add_plugins(EnginePlugin)
		.register_type::<Angle>()
		.insert_resource(RapierConfiguration {
			gravity: Vect::new(0.0, 0.0, -9.80665),
			timestep_mode: TimestepMode::Interpolated {
				dt: DT,
				time_scale: 1.0,
				substeps: 1,
			},
			..default()
		})
		.add_plugins((
			RapierPhysicsPlugin::<()>::default().in_schedule(Update),
			FrameTimeDiagnosticsPlugin,
			AudioPlugin,
		))
		.add_plugins((
			ParticlesPlugin,
			AbilitiesPlugin,
			OffloadingPlugin,
			SkyPlugin,
			MatsPlugin,
			anim::BuiltinAnimations,
			anim::AnimationPlugin::<Spewer>::PLUGIN,
			enemies::plugin.plugfn(),
			player::plugin.plugfn(),
			pickups::plugin.plugfn(),
			settings::plugin.plugfn(),
			planet::plugin.plugfn(),
			ui::plugin.plugfn(),
		))
		.insert_resource(PkvStore::new_with_qualifier("studio", "sonday", "has"))
		.add_systems(Startup, startup)
		.add_systems(
			First,
			shift_frame.before(planet::frame::reframe_all_entities),
		)
		.add_systems(
			Update,
			(terminal_velocity.before(StepSimulation), fullscreen),
		)
		.add_systems(PostUpdate, (despawn_oob,));

	app
}

/// The absolute furthest any entity can be away from the origin before being forcibly despawned.
/// Helps prevent crashes from `inf` translations or other bugs, as well as avoiding processing
/// irrelevant entities.
#[derive(Resource)]
pub struct AbsoluteBounds {
	pub extents: f32,
}

impl AbsoluteBounds {
	fn test(&self, point: Vec3) -> InBounds {
		point.x.abs() <= self.extents
			&& point.y.abs() <= self.extents
			&& point.z.abs() <= self.extents
	}
}

pub type InBounds = bool;

#[derive(Component, Debug)]
pub struct NeverDespawn;

fn despawn_oob(
	mut cmds: Commands,
	bounds: Res<AbsoluteBounds>,
	mut q: Query<(Entity, &mut GlobalTransform, Has<NeverDespawn>)>,
) {
	for (id, mut xform, never_despawn) in &mut q {
		if !bounds.test(xform.translation()) {
			let plan = if never_despawn {
				*xform = GlobalTransform::default();
				"Resetting transform to zero"
			} else {
				cmds.entity(id).despawn();
				"Despawning"
			};
			bevy::log::warn!("Entity {id:?} is way out of bounds. {plan}...");
		}
	}
}

#[derive(Component, Deref, DerefMut, Debug, Default, Reflect)]
pub struct TerminalVelocity(Velocity);

pub trait Ability: Component {
	fn cooldown(&mut self) -> Duration;
}

#[derive(Component, Debug, Default, Copy, Clone)]
#[component(storage = "SparseSet")]
pub struct Cooldown<A: Ability> {
	pub t: Duration,
	pub ability: Option<A>,
}

impl<A: Ability> Cooldown<A> {
	pub fn new(mut ability: A) -> Self {
		Self {
			t: ability.cooldown(),
			ability: Some(ability),
		}
	}
}

pub fn tick_cooldown<A: Ability>(
	mut cmds: Commands,
	mut q: Query<(Entity, &mut Cooldown<A>)>,
	t: Res<Time>,
) {
	for (id, mut cd) in q.iter_mut() {
		if let Some(t) = cd.t.checked_sub(t.delta()) {
			cd.t = t;
		} else {
			cmds.entity(id)
				.remove::<Cooldown<A>>()
				.insert(cd.ability.take().unwrap());
		}
	}
}

/// Keeps the Globals scene alive so change detection works.
#[derive(Resource, Deref, DerefMut)]
pub struct GlobalsScene(pub Handle<DynamicScene>);

fn startup(
	mut cmds: Commands,
	mut scene_spawner: ResMut<SceneSpawner>,
	assets: Res<AssetServer>,
	#[cfg(all(feature = "debugging", feature = "render"))] mut dbg_render_ctx: ResMut<
		DebugRenderContext,
	>,
) {
	let globals_scene = assets.load("globals.scn.ron");
	cmds.insert_resource(GlobalsScene(globals_scene.clone()));
	scene_spawner.spawn_dynamic(globals_scene);

	#[cfg(target_family = "wasm")]
	cmds.insert_resource(Msaa::Off);

	#[cfg(all(feature = "debugging", feature = "render"))]
	{
		dbg_render_ctx.enabled = false;
		dbg_render_ctx.pipeline.mode = DebugRenderMode::all();
	}

	cmds.insert_resource(AbsoluteBounds { extents: 65536.0 });

	cmds.insert_resource(AmbientLight {
		color: Color::rgb(0.64, 0.32, 1.0),
		brightness: 50.0,
		..default()
	});
}

fn terminal_velocity(mut q: Query<(&mut CtrlVel, &TerminalVelocity)>) {
	for (mut vel, term_vel) in q.iter_mut() {
		// Don't trigger vel.deref_mut if not necessary
		let term = term_vel.linvel;
		if vel.linvel.x.abs() > term.x {
			vel.linvel.x = term.x * vel.linvel.x.signum();
		}
		if vel.linvel.y.abs() > term.y {
			vel.linvel.y = term.y * vel.linvel.y.signum();
		}
		if vel.linvel.z.abs() > term.z {
			vel.linvel.z = term.z * vel.linvel.z.signum();
		}
		let term = term_vel.angvel;
		if vel.angvel.x.abs() > term.x {
			vel.angvel.x = term.x * vel.angvel.x.signum();
		}
		if vel.angvel.y.abs() > term.y {
			vel.angvel.y = term.y * vel.angvel.y.signum();
		}
		if vel.angvel.z.abs() > term.z {
			vel.angvel.z = term.z * vel.angvel.z.signum();
		}
	}
}

fn fullscreen(kb: Res<ButtonInput<KeyCode>>, mut windows: Query<&mut Window, With<PrimaryWindow>>) {
	use bevy::window::WindowMode::*;

	if kb.just_pressed(KeyCode::F11) {
		let mut window = windows.single_mut();
		window.mode = match window.mode {
			Windowed => BorderlessFullscreen,
			_ => Windowed,
		};
	}
}

#[derive(Component, Copy, Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Reflect)]
pub struct Alive;

pub fn shift_frame(
	player_q: Query<&GlobalTransform, WithVariant<Root>>,
	mut frame: ResMut<Frame>,
	mut prev_frame: ResMut<Prev<Frame>>,
) {
	if **prev_frame != *frame {
		**prev_frame = *frame;
	}
	let mut min = Vec2::NAN;
	let mut max = Vec2::NAN;
	for global in &player_q {
		let global = global.translation();
		min.x = f32::min(min.x, global.x);
		max.x = f32::max(max.x, global.x);
		min.y = f32::min(min.y, global.y);
		max.y = f32::max(max.y, global.y);
	}

	// Prepare to handle players being too far apart.
	// Should probably have separate frames for different players if possible, but
	// this is far easier to implement.
	const GROW: f32 = 1.5;
	// Should be less than `GROW * 0.5` in order to prevent oscillation when
	// `*_width` is multiplied by `GROW`
	const SHRINK_TRIGGER: f32 = 0.5;
	let mut grew = false;
	let x_width = max.x - min.x;
	if x_width >= frame.trigger_bounds {
		frame.trigger_bounds = x_width * GROW;
		grew = true;
	}
	let y_width = max.y - min.y;
	if y_width >= frame.trigger_bounds {
		frame.trigger_bounds = f32::max(frame.trigger_bounds, y_width * GROW);
		grew = true;
	}

	// Automatically shrink to recover precision
	if !grew
		&& x_width < frame.trigger_bounds * SHRINK_TRIGGER
		&& y_width < frame.trigger_bounds * SHRINK_TRIGGER
	{
		frame.trigger_bounds = f32::max(f32::max(x_width * GROW, y_width * GROW), frame.min_width);
	}

	let midpoint = Vec2::new((x_width * 0.5) + min.x, (y_width * 0.5) + min.y);

	// Actually move the frame if needed
	if midpoint.x.abs() >= frame.trigger_bounds {
		frame.center.x += midpoint.x.signum() as f64 * frame.trigger_bounds as f64;
	}
	if midpoint.y.abs() >= frame.trigger_bounds {
		frame.center.y += midpoint.y.signum() as f64 * frame.trigger_bounds as f64;
	}
}
