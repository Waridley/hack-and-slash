use crate::mats::BubbleMaterial;
use bevy::{
	diagnostic::FrameTimeDiagnosticsPlugin,
	ecs::system::EntityCommands,
	prelude::*,
	render::mesh::{PrimitiveTopology, VertexAttributeValues::Float32x3},
	DefaultPlugins,
};
use bevy_common_assets::ron::RonAssetPlugin;
use bevy_kira_audio::AudioPlugin;
use bevy_rapier3d::{parry::shape::SharedShape, prelude::*};
use particles::ParticlesPlugin;
use player::ctrl::CtrlVel;
use rapier3d::{
	geometry::HeightField,
	na::{DMatrix, Vector3},
};
use std::{f32::consts::*, fmt::Debug, sync::Arc, time::Duration};
use util::FnPluginExt;

pub mod mats;
pub mod pickups;
pub mod player;
pub mod settings;
pub mod ui;
pub mod util;
#[cfg(feature = "testing")]
pub mod tests;

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
		.fn_plugin(player::plugin)
		.fn_plugin(pickups::plugin)
		.fn_plugin(ui::plugin)
		.add_plugin(RonAssetPlugin::<BubbleMaterial>::new(&["mat.ron"]))
		.add_plugin(MaterialPlugin::<BubbleMaterial>::default())
		.add_startup_system(startup)
		.add_system(terminal_velocity)
		.add_system(fullscreen);

	#[cfg(debug_assertions)]
	{
		app
			.add_plugin(RapierDebugRenderPlugin::default())
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

#[derive(Component, Debug, Default, Clone, Copy)]
struct IsPlayerXformText;

fn startup(
	mut cmds: Commands,
	mut materials: ResMut<Assets<StandardMaterial>>,
	mut meshes: ResMut<Assets<Mesh>>,
	#[cfg(debug_assertions)]
	mut dbg_render_ctx: ResMut<DebugRenderContext>,
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
	let material = materials.add(StandardMaterial {
		base_color: Color::rgb(0.024, 0.0, 0.064),
		reflectance: 0.032,
		..default()
	});

	use noises_and_patterns::{noise::Noise, FP};
	let noise = noises_and_patterns::noise::value::Value::new();
	let r = 48;
	let d = r * 2;
	let (columns, rows) = (d, d);

	let heights = (0..(rows * columns))
		.map(|i: usize| {
			let col = i % d;
			let row = i / d;

			let r = r as f32 - 2.0;
			let x = (col as f32 - 1.0) - r;
			let y = (row as f32 - 1.0) - r;

			// sphere
			let bowl = ((r * r) - (x * x) - (y * y)).sqrt() / r;

			// rounded cube might as well add real walls, looks too obvious that it's a heightfield
			// let bowl = ((r * r * r * r) - (x * x * x * x) - (y * y * y * y)).sqrt().sqrt() / r;

			let bowl = if bowl.is_finite() { bowl } else { 0.0 };

			noise.fbm_2d((row as FP * 0.3, (i % rows) as FP * 0.3), 3) - (bowl * 20.0)
		}) // scale to -1.0..=1.0
		.collect();

	let heightfield = HeightField::new(
		DMatrix::from_vec(rows, columns, heights),
		Vector3::new(1024.0, 24.0, 1024.0),
	);
	let tris = heightfield.triangles();

	let mut mesh = Mesh::new(PrimitiveTopology::TriangleList);
	let vertices = tris
		.flat_map(|pos| {
			[
				[pos.a.x, pos.a.y, pos.a.z],
				[pos.b.x, pos.b.y, pos.b.z],
				[pos.c.x, pos.c.y, pos.c.z],
			]
		})
		.collect::<Vec<_>>();
	mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, Float32x3(vertices));
	mesh.compute_flat_normals();
	let mesh = meshes.add(mesh);

	cmds.spawn((
		RigidBody::Fixed,
		Collider::from(SharedShape(Arc::new(heightfield))),
		MaterialMeshBundle::<StandardMaterial> {
			mesh,
			transform: Transform {
				translation: Vec3::new(0.0, 0.0, 256.0),
				rotation: Quat::from_rotation_x(FRAC_PI_2),
				..default()
			},
			material: material.clone(),
			..default()
		},
	));

	let mesh = Mesh::from(shape::Cube { size: 64.0 });
	let collider = Collider::cuboid(32.0, 32.0, 32.0);
	let mesh = meshes.add(mesh);

	let mut factory = TerrainFactory {
		cmds: &mut cmds,
		mesh,
		material,
		collider,
	};

	factory.spawn(Transform::from_translation(Vec3::new(0.0, 0.0, -48.0)));
	factory.spawn(Transform {
		translation: Vec3::new(-40.0, -8.0, -44.0),
		rotation: Quat::from_euler(EulerRot::ZXY, 0.0, 0.0, -FRAC_PI_3),
		..default()
	});
	factory.spawn(Transform {
		translation: Vec3::new(42.0, 24.0, -36.0),
		rotation: Quat::from_rotation_y(FRAC_PI_4),
		..default()
	});
	factory.spawn(Transform::from_translation(Vec3::new(-32.0, 42.0, -44.0)));
	factory.spawn(Transform::from_translation(Vec3::new(-64.0, 72.0, -32.0)));
	factory.spawn(Transform::from_translation(Vec3::new(-96.0, 0.0, -64.0)));
	factory.spawn(Transform {
		translation: Vec3::new(-24.0, 96.0, 16.0),
		rotation: Quat::from_rotation_x(FRAC_PI_3),
		..default()
	});
}

/// Share mesh, material, and collider amongst multiple `TerrainObjects`
pub struct TerrainFactory<'c, 'w: 'c, 's: 'c> {
	pub cmds: &'c mut Commands<'w, 's>,
	pub mesh: Handle<Mesh>,
	pub material: Handle<StandardMaterial>,
	pub collider: Collider,
}

impl<'c, 'w: 'c, 's: 'c> TerrainFactory<'c, 'w, 's> {
	fn spawn<'a>(&'a mut self, transform: Transform) -> EntityCommands<'w, 's, 'a> {
		self.cmds.spawn(TerrainObject {
			mat_mesh_bundle: MaterialMeshBundle {
				mesh: self.mesh.clone(),
				material: self.material.clone(),
				transform,
				..default()
			},
			collider: self.collider.clone(),
			..default()
		})
	}
}

#[derive(Bundle)]
pub struct TerrainObject {
	pub mat_mesh_bundle: MaterialMeshBundle<StandardMaterial>,
	pub rigid_body: RigidBody,
	pub collider: Collider,
	pub restitution: Restitution,
	pub friction: Friction,
	pub ccd: Ccd,
}

impl Default for TerrainObject {
	fn default() -> Self {
		Self {
			mat_mesh_bundle: MaterialMeshBundle::default(),
			rigid_body: RigidBody::Fixed,
			collider: Collider::default(),
			restitution: Restitution::new(0.5),
			friction: Friction::new(0.01),
			ccd: Ccd::disabled(),
		}
	}
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
