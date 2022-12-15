use crate::mats::BubbleMaterial;
use bevy::{
	diagnostic::FrameTimeDiagnosticsPlugin,
	prelude::*,
	DefaultPlugins,
};
use bevy_common_assets::ron::RonAssetPlugin;
use bevy_kira_audio::AudioPlugin;
use bevy_pkv::PkvStore;
use bevy_rapier3d::{prelude::*};
use particles::ParticlesPlugin;
use player::ctrl::CtrlVel;
use std::{f32::consts::*, fmt::Debug, time::Duration};
use util::FnPluginExt;

pub mod enemies;
pub mod mats;
pub mod pickups;
pub mod player;
pub mod settings;
pub mod terrain;
#[cfg(feature = "testing")]
pub mod tests;
pub mod ui;
pub mod util;

/// Epsilon
pub const E: f32 = 1.0e-5;
/// Rotational epsilon in radians
pub const R_E: f32 = TAU / (360.0 * 4.0);
/// Fixed delta time
pub const DT: f32 = 1.0 / 64.0;
/// Up vector
pub const UP: Vect = Vect::Z;

pub fn run() {
	let mut app = App::new();
	let mut default_plugins = DefaultPlugins.set(WindowPlugin {
		window: WindowDescriptor {
			title: "Sonday Hack-and-Slash Game".to_string(),
			resizable: true,
			fit_canvas_to_parent: true,
			canvas: Some("#game_canvas".into()),
			..default()
		},
		..default()
	});
	#[cfg(debug_assertions)]
	{
		default_plugins = default_plugins.set(AssetPlugin {
			watch_for_changes: true,
			..default()
		});
	}
	app.add_plugins(default_plugins)
		.insert_resource(RapierConfiguration {
			gravity: Vect::new(0.0, 0.0, -9.81),
			// timestep_mode: TimestepMode::Fixed {
			// 	dt: DT,
			// 	substeps: 1,
			// },
			..default()
		})
		.add_plugin(RapierPhysicsPlugin::<()>::default())
		.add_plugin(FrameTimeDiagnosticsPlugin::default())
		.add_plugin(AudioPlugin)
		.add_plugin(ParticlesPlugin)
		.insert_resource(PkvStore::new_with_qualifier(
			"studio",
			"sonday",
			env!("CARGO_PKG_NAME"),
		))
		.fn_plugin(enemies::plugin)
		.fn_plugin(player::plugin)
		.fn_plugin(pickups::plugin)
		.fn_plugin(settings::plugin)
		.fn_plugin(terrain::plugin)
		.fn_plugin(ui::plugin)
		.add_plugin(RonAssetPlugin::<BubbleMaterial>::new(&["mat.ron"]))
		.add_plugin(MaterialPlugin::<BubbleMaterial>::default())
		.add_startup_system(startup)
		.add_system(terminal_velocity)
		.add_system(fullscreen);

	#[cfg(debug_assertions)]
	{
		app.add_plugin(RapierDebugRenderPlugin::default())
			.add_system(toggle_debug_rendering);
	}

	app.run()
}

#[derive(Resource)]
pub struct AbsoluteBounds {
	extents: f32,
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
	#[cfg(debug_assertions)] mut dbg_render_ctx: ResMut<DebugRenderContext>,
) {
	#[cfg(target_family = "wasm")]
	cmds.insert_resource(Msaa { samples: 1 }); // disables MSAA

	#[cfg(debug_assertions)]
	{
		dbg_render_ctx.enabled = false;
	}

	cmds.insert_resource(AbsoluteBounds { extents: 2048.0 });

	cmds.spawn(DirectionalLightBundle {
		directional_light: DirectionalLight {
			// illuminance: 10000.0,
			..default()
		},
		transform: Transform::from_rotation(Quat::from_rotation_arc(
			Vec3::NEG_Z,
			Vec3::new(-1.0, 0.5, -1.0).normalize(),
		)),
		..default()
	});

	cmds.insert_resource(AmbientLight {
		// brightness: 0.2,
		..default()
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

fn fullscreen(kb: Res<Input<KeyCode>>, mut windows: ResMut<Windows>) {
	use WindowMode::*;

	if kb.just_pressed(KeyCode::F11) {
		let window = windows.get_primary_mut().unwrap();
		window.set_mode(match window.mode() {
			Windowed => BorderlessFullscreen,
			_ => Windowed,
		})
	}
}

#[cfg(debug_assertions)]
fn toggle_debug_rendering(mut ctx: ResMut<DebugRenderContext>, input: Res<Input<KeyCode>>) {
	if input.just_pressed(KeyCode::P) {
		ctx.enabled = !ctx.enabled
	}
}
