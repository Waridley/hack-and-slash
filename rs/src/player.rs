use crate::{terminal_velocity, AbsoluteBounds, TerminalVelocity, R_E};
use bevy::{
	ecs::system::EntityCommands,
	prelude::{
		shape::{Icosphere, RegularPolygon},
		CoreStage::PreUpdate,
		*,
	},
};
use bevy_rapier3d::{
	control::KinematicCharacterController,
	dynamics::{CoefficientCombineRule::Min, RigidBody, Velocity},
	geometry::{Collider, Friction},
	math::Vect,
	prelude::{RigidBody::KinematicPositionBased, *},
};
use camera::spawn_camera;
use ctrl::CtrlVel;
use enum_components::{EntityEnumCommands, EnumComponent};
use leafwing_input_manager::prelude::*;
use nanorand::Rng;
use particles::{
	update::{Linear, TargetScale},
	InitialGlobalTransform, InitialTransform, Lifetime, ParticleBundle, Spewer, SpewerBundle,
};
use std::{
	f32::consts::*,
	num::NonZeroU8,
	ops::{Deref, DerefMut},
	time::Duration,
};

pub mod camera;
pub mod ctrl;
pub mod input;
pub mod prefs;

pub const MAX_SPEED: f32 = 64.0;
pub const ACCEL: f32 = 3.0;
pub const PLAYER_GRAVITY: f32 = 64.0;
pub const MAX_JUMPS: f32 = 2.0;
pub const JUMP_VEL: f32 = 64.0;
pub const CLIMB_ANGLE: f32 = FRAC_PI_3 - R_E;
pub const SLIDE_ANGLE: f32 = FRAC_PI_3 - R_E;
pub const HOVER_HEIGHT: f32 = 2.0;
const G1: rapier3d::geometry::Group = rapier3d::geometry::Group::GROUP_1;

pub fn plugin(app: &mut App) -> &mut App {
	app.fn_plugin(input::plugin)
		.add_startup_system(setup)
		.add_system_to_stage(PreUpdate, ctrl::gravity)
		.add_system_to_stage(PreUpdate, ctrl::repel_ground.after(ctrl::gravity))
		// .add_system(tick_cooldown::<Jump>)
		.add_system_to_stage(CoreStage::PreUpdate, ctrl::reset_jump_on_ground)
		.add_system(input::movement_input.before(terminal_velocity))
		.add_system(input::look_input.before(terminal_velocity))
		.add_system(camera::position_target.after(input::look_input))
		.add_system(camera::follow_target.after(camera::position_target))
		.add_system(ctrl::move_player.after(terminal_velocity))
		.add_system(idle)
		.add_system_to_stage(CoreStage::Last, reset_oob)
}

fn setup(
	mut cmds: Commands,
	mut materials: ResMut<Assets<StandardMaterial>>,
	mut meshes: ResMut<Assets<Mesh>>,
	asset_server: Res<AssetServer>,
	settings: Res<Settings>,
) {
	let id = unsafe { PlayerId::new_unchecked(1) };
	spawn_camera(&mut cmds, id, &*settings);

	let aoe_sfx = asset_server.load("sfx/SFX_-_magic_spell_03.ogg");
	cmds.insert_resource(AoESound(aoe_sfx));

	let ship = asset_server.load("ships/player.glb#Scene0");
	let vis = SceneBundle {
		scene: ship,
		// transform: Transform {
		// 	translation: Vec3::new(-1.875, -0.625, 0.25),
		// 	rotation: Quat::from_rotation_y(FRAC_PI_4),
		// 	scale: Vec3::splat(0.75),
		// },
		..default()
	};

	let particle_mesh = Mesh::from(shape::Torus {
		radius: 0.640,
		ring_radius: 0.064,
		subdivisions_segments: 6,
		subdivisions_sides: 3,
	});
	let particle_mesh = meshes.add(particle_mesh);
	let particle_material = materials.add(StandardMaterial {
		// base_color: Color::rgba(0.0, 1.0, 0.5, 0.3),
		base_color: Color::NONE,
		emissive: Color::rgb(0.0, 4.0, 2.4),
		reflectance: 0.0,
		..default()
	});

	let particle_mesh = MaterialMeshBundle {
		mesh: particle_mesh,
		material: particle_material,
		..default()
	};

	let arm = Mesh::from(Icosphere {
		radius: 0.3,
		subdivisions: 2,
	});
	let arm_mesh = meshes.add(arm);
	let arm1 = MaterialMeshBundle::<StandardMaterial> {
		mesh: arm_mesh.clone(),
		material: materials.add(StandardMaterial {
			base_color: Color::NONE,
			emissive: Color::GREEN * 4.0,
			reflectance: 0.0,
			double_sided: true,
			cull_mode: None,
			..default()
		}),
		transform: Transform::from_translation(Vec3::X * 2.0),
		..default()
	};
	let arm2 = MaterialMeshBundle::<StandardMaterial> {
		mesh: arm_mesh.clone(),
		material: materials.add(StandardMaterial {
			base_color: Color::NONE,
			emissive: Color::WHITE * 4.0,
			reflectance: 0.0,
			double_sided: true,
			cull_mode: None,
			..default()
		}),
		transform: Transform::from_translation(
			Quat::from_rotation_z(FRAC_PI_3 * 2.0) * Vec3::X * 2.0,
		),
		..default()
	};
	let arm3 = MaterialMeshBundle::<StandardMaterial> {
		mesh: arm_mesh,
		material: materials.add(StandardMaterial {
			base_color: Color::NONE,
			emissive: Color::CYAN * 4.0,
			reflectance: 0.0,
			double_sided: true,
			cull_mode: None,
			..default()
		}),
		transform: Transform::from_translation(
			Quat::from_rotation_z(FRAC_PI_3 * 4.0) * Vec3::X * 2.0,
		),
		..default()
	};

	let arm_particle_mesh = meshes.add(Mesh::from(RegularPolygon::new(0.05, 3)));

	use PlayerArm::*;
	cmds.spawn_player(
		id,
		vis,
		particle_mesh,
		[(arm1, A), (arm2, B), (arm3, C)],
		arm_particle_mesh,
	);
}

#[derive(EnumComponent)]
pub enum PlayerEntity {
	Root,
	Controller,
	Vis,
	VisNode,
	CamPivot,
	Cam,
	HoverParticle,
	Arm(PlayerArm),
	OrbitalParticle,
}
use crate::{
	player::{
		input::{AoESound, PlayerAction},
		prefs::PlayerPrefs,
	},
	util::FnPluginExt,
};
use player_entity::*;
use crate::settings::Settings;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum PlayerArm {
	A,
	B,
	C,
}

pub type PlayerId = NonZeroU8;

#[derive(Component, Debug, Clone, Copy, PartialEq, Eq)]
pub struct BelongsToPlayer(PlayerId);

impl BelongsToPlayer {
	pub fn new(id: u8) -> Self {
		Self(PlayerId::new(id).unwrap())
	}

	pub fn with_id(id: PlayerId) -> Self {
		Self(id)
	}

	pub fn id(&self) -> PlayerId {
		self.0
	}
}

pub trait SpawnPlayer<'c, 'w: 'c, 's: 'c> {
	fn spawn_player(
		&'c mut self,
		id: PlayerId,
		scene: SceneBundle,
		particle_mesh: MaterialMeshBundle<StandardMaterial>,
		arm_meshes: [(MaterialMeshBundle<StandardMaterial>, PlayerArm); 3],
		arm_particle_mesh: Handle<Mesh>,
	) -> EntityCommands<'w, 's, 'c>;
}

impl<'c, 'w: 'c, 's: 'c> SpawnPlayer<'c, 'w, 's> for Commands<'w, 's> {
	fn spawn_player(
		&'c mut self,
		id: PlayerId,
		vis: SceneBundle,
		particle_mesh: MaterialMeshBundle<StandardMaterial>,
		arm_meshes: [(MaterialMeshBundle<StandardMaterial>, PlayerArm); 3],
		arm_particle_mesh: Handle<Mesh>,
	) -> EntityCommands<'w, 's, 'c> {
		let owner = BelongsToPlayer::with_id(id);
		let char_collider = Collider::ball(1.4);
		let mut root = self.spawn((
			owner,
			TerminalVelocity(Velocity {
				linvel: Vect::splat(128.0),
				angvel: Vect::new(0.0, 0.0, TAU * 60.0), // one rotation per frame at 60 fps
			}),
			RigidBody::KinematicPositionBased,
			TransformBundle::default(),
			Velocity::default(),
			Friction {
				coefficient: 0.0,
				combine_rule: Min,
			},
			VisibilityBundle::default(),
		));
		root.set_enum(PlayerEntity::Root);

		root.with_children(|builder| {
			builder
				.spawn((
					owner,
					char_collider,
					TransformBundle::default(),
					CtrlVel::default(),
					CollisionGroups::new(Group::GROUP_1, !Group::GROUP_1),
					KinematicCharacterController {
						up: Vect::Z,
						snap_to_ground: None,
						autostep: None,
						max_slope_climb_angle: CLIMB_ANGLE,
						min_slope_slide_angle: SLIDE_ANGLE,
						filter_flags: QueryFilterFlags::EXCLUDE_SENSORS,
						filter_groups: Some(InteractionGroups::new(G1, !G1)),
						..default()
					},
					InputManagerBundle::<PlayerAction> {
						input_map: PlayerPrefs::default().input_map,
						..default()
					},
					PlayerAction::abilities_bundle(),
				))
				.set_enum(PlayerEntity::Controller);
		});
		player_vis(&mut root, owner, vis, particle_mesh);
		player_arms(&mut root, owner, arm_meshes, arm_particle_mesh);
		root
	}
}

fn player_vis(
	cmds: &mut EntityCommands,
	owner: BelongsToPlayer,
	vis: SceneBundle,
	particle_mesh: MaterialMeshBundle<StandardMaterial>,
) {
	let pivot = camera::spawn_pivot(cmds.commands(), owner).id();
	let vis_node = cmds
		.commands()
		.spawn((
			owner,
			TransformBundle::from_transform(Transform {
				translation: Vec3::NEG_Z * 0.64,
				rotation: Quat::from_rotation_x(FRAC_PI_2),
				..default()
			}),
			// TransformBundle::default(),
			VisibilityBundle::default(), // for children ComputedVisibility
		))
		.set_enum(PlayerEntity::VisNode)
		.with_children(move |builder| {
			// Mesh is a child so we can apply transform independent of collider to align them
			builder
				.spawn((
					owner,
					TransformBundle::default(),
					VisibilityBundle::default(),
				))
				.set_enum(PlayerEntity::Vis)
				.with_children(|builder| {
					builder.spawn((owner, vis));

					let transform = Transform {
						// rotation: Quat::from_rotation_x(FRAC_PI_2),
						..default()
					};
					let particle_mesh = MaterialMeshBundle {
						transform,
						..particle_mesh
					};

					let mut rng = nanorand::WyRand::new();
					builder.spawn((
						owner,
						SpewerBundle {
							spewer: Spewer {
								factory: Box::new(move |cmds, xform, time_created| {
									let xform = xform.compute_transform();
									let xform = Transform {
										translation: Vec3 {
											z: xform.translation.z - rng.generate::<f32>() * 0.2,
											..xform.translation
										},
										..xform
									};
									cmds.spawn((
										ParticleBundle {
											mesh_bundle: MaterialMeshBundle {
												transform: xform,
												..particle_mesh.clone()
											},
											lifetime: Lifetime(Duration::from_secs_f32(0.32)),
											initial_transform: InitialTransform(xform),
											time_created,
											..default()
										},
										Linear {
											velocity: Vec3::NEG_Z * 5.0,
										},
										TargetScale {
											scale: Vec3::splat(0.1),
										},
									))
								}),
								interval: Duration::from_secs_f32(0.033),
								// jitter: Duration::from_secs_f32(0.033),
								global_coords: true,
								..default()
							},
							transform: TransformBundle::from_transform(Transform {
								translation: Vec3::new(0.0, -0.5, 0.0),
								..default()
							}),
							..default()
						},
					));
				});
		})
		.add_child(pivot)
		.id();
	cmds.add_child(vis_node);
}

#[derive(Component, Debug, Default, Copy, Clone, Reflect, FromReflect)]
pub struct RotVel {
	pub quiescent: f32,
	pub current: f32,
}

impl RotVel {
	fn new(vel: f32) -> Self {
		Self {
			quiescent: vel,
			current: vel,
		}
	}
}

impl Deref for RotVel {
	type Target = f32;

	fn deref(&self) -> &Self::Target {
		&self.current
	}
}

impl DerefMut for RotVel {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.current
	}
}

fn player_arms(
	cmds: &mut EntityCommands,
	owner: BelongsToPlayer,
	meshes: [(MaterialMeshBundle<StandardMaterial>, PlayerArm); 3],
	particle_mesh: Handle<Mesh>,
) {
	cmds.with_children(|builder| {
		for (arm, which) in meshes {
			let particle_mesh = particle_mesh.clone();
			let particle_mat = arm.material.clone();
			let mut rng = nanorand::WyRand::new();
			let spewer = Spewer {
				factory: Box::new(move |cmds, xform: &GlobalTransform, time_created| {
					let mut xform = xform.compute_transform();
					xform.translation.x += rng.generate::<f32>() * 0.7 - 0.35;
					xform.translation.y += rng.generate::<f32>() * 0.7 - 0.35;
					xform.translation.z += rng.generate::<f32>() * 0.7 - 0.35;

					// // not working ?:/
					// xform.rotation = Quat::from_scaled_axis(Vec3::new(
					// 	rng.f32() * TAU,
					// 	rng.f32() * TAU,
					// 	rng.f32() * TAU,
					// ));

					cmds.spawn((
						ParticleBundle {
							mesh_bundle: MaterialMeshBundle {
								mesh: particle_mesh.clone(),
								material: particle_mat.clone(),
								transform: xform,
								global_transform: xform.into(),
								..default()
							},
							time_created,
							initial_transform: InitialTransform(xform),
							initial_global_transform: InitialGlobalTransform(xform.into()),
							lifetime: Lifetime(Duration::from_secs_f32(0.256)),
						},
						Linear {
							velocity: Vec3::NEG_Z * 5.0,
						},
					))
				}),
				interval: Duration::from_millis(2),
				global_coords: true,
				..default()
			};
			builder
				.spawn((
					owner,
					arm,
					spewer,
					Collider::ball(0.4),
					Sensor,
					KinematicPositionBased,
					RotVel::new(8.0),
				))
				.set_enum(PlayerEntity::Arm(which));
		}
	});
}

pub fn reset_oob(
	mut cmds: Commands,
	q: Query<(Entity, &GlobalTransform, PlayerEntity, &BelongsToPlayer)>,
	bounds: Res<AbsoluteBounds>,
) {
	let mut to_respawn = vec![];
	for (_id, xform, which, owner) in &q {
		if let PlayerEntityItem::Root(..) = which {
			if xform.translation().x.abs() > bounds.extents
				|| xform.translation().y.abs() > bounds.extents
				|| xform.translation().z.abs() > bounds.extents
			{
				to_respawn.push(owner)
			}
		}
	}
	for (id, _, _, owner) in &q {
		if to_respawn.contains(&owner) {
			cmds.entity(id).despawn_recursive();
			todo!("respawn player")
		}
	}
}

pub fn idle(
	mut vis_q: Query<&mut Transform, ReadPlayerEntity<Vis>>,
	mut arm_q: Query<(&mut Transform, &RotVel), ReadPlayerEntity<Arm>>,
	t: Res<Time>,
) {
	for mut xform in &mut vis_q {
		xform.translation.y = (t.elapsed_seconds() * 3.0).sin() * 0.24;
		xform.rotate_local_y(-2.0 * t.delta_seconds());
	}
	for (mut xform, rvel) in &mut arm_q {
		xform.translation = Quat::from_rotation_z(t.delta_seconds() * **rvel)
			* Vec3 {
				z: (t.elapsed_seconds() + 2.0 * xform.translation.angle_between(Vec3::X)).sin(),
				..xform.translation
			}
	}
}
