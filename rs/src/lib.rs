use crate::mats::BubbleMaterial;
use bevy::{
	diagnostic::FrameTimeDiagnosticsPlugin, prelude::*, render::RenderPlugin, window::PrimaryWindow,
};
use bevy_common_assets::ron::RonAssetPlugin;
use bevy_kira_audio::AudioPlugin;
use bevy_pkv::PkvStore;
use bevy_rapier3d::prelude::*;
use particles::ParticlesPlugin;
use player::ctrl::CtrlVel;
use std::{f32::consts::*, fmt::Debug, time::Duration};
use util::IntoFnPlugin;

#[allow(unused_imports, clippy::single_component_path_imports)]
#[cfg(all(debug_assertions, not(target_arch = "wasm32")))]
use bevy_dylib;

pub mod enemies;
pub mod mats;
pub mod nav;
pub mod offloading;
pub mod pickups;
pub mod planet;
pub mod player;
pub mod settings;
#[cfg(feature = "testing")]
pub mod tests;
pub mod ui;
pub mod util;

/// Epsilon
pub const EPS: f32 = 1.0e-5;
/// Rotational epsilon in radians
pub const R_EPS: f32 = TAU / (360.0 * 4.0);
/// Fixed delta time
pub const DT: f32 = 1.0 / 128.0;
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

		game_plugin(app);
	}
}

pub fn game_plugin(app: &mut App) -> &mut App {
	app.insert_resource(RapierConfiguration {
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
		ParticlesPlugin,
		offloading::OffloadingPlugin,
		planet::sky::SkyPlugin,
		enemies::plugin.plugfn(),
		player::plugin.plugfn(),
		pickups::plugin.plugfn(),
		settings::plugin.plugfn(),
		planet::plugin.plugfn(),
		ui::plugin.plugfn(),
	))
	.insert_resource(PkvStore::new_with_qualifier(
		"studio",
		"sonday",
		env!("CARGO_PKG_NAME"),
	))
	.add_plugins((
		RonAssetPlugin::<BubbleMaterial>::new(&["mat.ron"]),
		MaterialPlugin::<BubbleMaterial>::default(),
	))
	.add_systems(Startup, startup)
	.add_systems(Update, (terminal_velocity, fullscreen))
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

fn despawn_oob(mut cmds: Commands, bounds: Res<AbsoluteBounds>, q: Query<(Entity, &Transform)>) {
	for (id, xform) in &q {
		if !bounds.test(xform.translation) {
			bevy::log::warn!("Entity {id:?} is way out of bounds. Despawning.");
			cmds.entity(id).despawn()
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

fn startup(
	mut cmds: Commands,
	#[cfg(all(feature = "debugging", feature = "render"))] mut dbg_render_ctx: ResMut<
		DebugRenderContext,
	>,
) {
	#[cfg(target_family = "wasm")]
	cmds.insert_resource(Msaa::Off);

	#[cfg(all(feature = "debugging", feature = "render"))]
	{
		dbg_render_ctx.enabled = false;
	}

	cmds.insert_resource(AbsoluteBounds { extents: 65536.0 });

	cmds.insert_resource(AmbientLight {
		color: Color::rgb(0.64, 0.32, 1.0),
		brightness: 0.1,
		// // If using EnvironmentLight diffuse instead, must still insert ambient light so a default one is not used
		// brightness: 0.0,
	});
}

fn terminal_velocity(mut q: Query<(&mut CtrlVel, &TerminalVelocity)>) {
	for (mut vel, term_vel) in q.iter_mut() {
		// Don't trigger vel.deref_mut if not necessary
		let term = term_vel.linvel;
		if vel.linvel.x.abs() > term.x {
			vel.linvel.x = term.x * vel.linvel.x.signum()
		}
		if vel.linvel.y.abs() > term.y {
			vel.linvel.y = term.y * vel.linvel.y.signum()
		}
		if vel.linvel.z.abs() > term.z {
			vel.linvel.z = term.z * vel.linvel.z.signum()
		}
		let term = term_vel.angvel;
		if vel.angvel.x.abs() > term.x {
			vel.angvel.x = term.x * vel.angvel.x.signum()
		}
		if vel.angvel.y.abs() > term.y {
			vel.angvel.y = term.y * vel.angvel.y.signum()
		}
		if vel.angvel.z.abs() > term.z {
			vel.angvel.z = term.z * vel.angvel.z.signum()
		}
	}
}

fn fullscreen(kb: Res<Input<KeyCode>>, mut windows: Query<&mut Window, With<PrimaryWindow>>) {
	use bevy::window::WindowMode::*;

	if kb.just_pressed(KeyCode::F11) {
		let mut window = windows.single_mut();
		window.mode = match window.mode {
			Windowed => BorderlessFullscreen,
			_ => Windowed,
		};
	}
}
