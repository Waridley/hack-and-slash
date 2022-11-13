use crate::input::InputPlugin;
use crate::player::PlayerControllerPlugin;
use bevy::{
	diagnostic::FrameTimeDiagnosticsPlugin,
	ecs::system::EntityCommands,
	prelude::{CoreStage::*, *},
	window::close_on_esc,
	DefaultPlugins,
};
use bevy_rapier3d::prelude::*;
use input::CtrlVel;
use particles::ParticlesPlugin;
use std::{
	f32::consts::*,
	fmt::{Debug, Formatter},
	time::Duration,
};

pub mod input;
pub mod player;

/// Epsilon
pub const E: f32 = 1.0e-4;
/// Fixed delta time
pub const DT: f32 = 1.0 / 64.0;

#[bevy_main]
pub fn main() {
	let mut app = App::new();
	app.add_plugins(DefaultPlugins.set(WindowPlugin {
		window: WindowDescriptor {
			title: "Sonday Hack-and-Slash Game".to_string(),
			resizable: true,
			fit_canvas_to_parent: true,
			..default()
		},
		..default()
	}))
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
	.add_plugin(PlayerControllerPlugin)
	.add_plugin(InputPlugin)
	.add_plugin(ParticlesPlugin)
	.add_startup_system(startup)
	.add_system(terminal_velocity)
	// .add_system_to_stage(PostUpdate, kill_oob)
	// .add_system_to_stage(Last, despawn_dead)
	;

	#[cfg(not(target_arch = "wasm32"))]
	{
		app.add_system(fullscreen)
			.add_system_to_stage(Last, close_on_esc);
	}

	#[cfg(debug_assertions)]
	{
		app.add_plugin(RapierDebugRenderPlugin::default())
			.add_system(toggle_debug_rendering);
	}

	app.run()
}

#[derive(Resource)]
struct AbsoluteBounds {
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
) {
	cmds.insert_resource(AbsoluteBounds { extents: 1024.0 });

	cmds.spawn(DirectionalLightBundle {
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

	let mesh = Mesh::from(shape::Cube { size: 64.0 });
	let collider = Collider::cuboid(32.0, 32.0, 32.0);
	let mesh = meshes.add(mesh);
	let material = materials.add(Color::rgb(0.1, 0.0, 0.2).into());

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

// #[derive(Component)]
// pub struct Despawner {
// 	dead: bool,
// 	despawn: Box<dyn for<'w, 's, 'a> FnMut(EntityCommands<'w, 's, 'a>) + Send + Sync + 'static>,
// }
//
// impl Despawner {
// 	pub fn new(
// 		despawn: impl for<'w, 's, 'a> FnMut(EntityCommands<'w, 's, 'a>) + Send + Sync + 'static,
// 	) -> Self {
// 		Self {
// 			dead: false,
// 			despawn: Box::new(despawn),
// 		}
// 	}
//
// 	pub fn run_if_dead(&mut self, cmds: EntityCommands) {
// 		if self.dead {
// 			(self.despawn)(cmds);
// 		}
// 	}
//
// 	pub fn kill(&mut self) {
// 		self.dead = true;
// 	}
//
// 	pub fn kill_now(&mut self, cmds: EntityCommands) {
// 		self.dead = true;
// 		(self.despawn)(cmds);
// 	}
//
// 	pub fn is_dead(&self) -> bool {
// 		self.dead
// 	}
// }
//
// impl Debug for Despawner {
// 	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
// 		f.debug_struct(std::any::type_name::<Self>())
// 			.field("dead", &self.dead)
// 			.finish()
// 	}
// }
//
// impl Default for Despawner {
// 	fn default() -> Self {
// 		Self::new(|cmds: EntityCommands| cmds.despawn_recursive())
// 	}
// }

// fn kill_oob(
// 	mut cmds: Commands,
// 	mut q: Query<(Entity, &GlobalTransform, Option<&mut Despawner>, Option<&Parent>)>,
// 	bounds: Res<AbsoluteBounds>,
// ) {
// 	for (id, xform, mut death_handler, parent) in &mut q {
// 		if xform.translation().x.abs() > bounds.extents
// 			|| xform.translation().y.abs() > bounds.extents
// 			|| xform.translation().z.abs() > bounds.extents
// 		{
// 			if let Some(mut death_handler) = death_handler {
// 				death_handler.kill()
// 			} else if parent.is_none() {
// 				cmds.entity(id).despawn()
// 			}
// 		}
// 	}
// }

// fn despawn_dead(mut cmds: Commands, mut q: Query<(Entity, &mut Despawner)>) {
// 	for (id, mut handler) in q.iter_mut() {
// 		handler.run_if_dead(cmds.entity(id))
// 	}
// }

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

#[cfg(test)]
mod tests {}
