use crate::input::PlayerAction;
use crate::{input, terminal_velocity, AbsoluteBounds, TerminalVelocity, R_E};
use bevy::{
	ecs::system::EntityCommands,
	prelude::{CoreStage::PreUpdate, *},
};
use bevy_rapier3d::plugin::systems::update_character_controls;
use bevy_rapier3d::{
	control::{KinematicCharacterController, KinematicCharacterControllerOutput},
	dynamics::{CoefficientCombineRule::Min, RigidBody, Velocity},
	geometry::{Collider, Friction},
	math::Vect,
	prelude::*,
};
use camera::spawn_camera;
use ctrl::CtrlVel;
use enum_components::{EntityEnumCommands, EnumComponent};
use leafwing_abilities::prelude::*;
use leafwing_input_manager::prelude::*;
use particles::{
	update::{Linear, TargetScale},
	InitialTransform, Lifetime, ParticleBundle, Spewer, SpewerBundle,
};
use rapier3d::control::{CharacterAutostep, CharacterLength};
use std::{f32::consts::*, num::NonZeroU8, sync::Arc, time::Duration};

pub mod camera;
pub mod ctrl;

pub const MAX_SPEED: f32 = 64.0;
pub const ACCEL: f32 = 3.0;
pub const PLAYER_GRAVITY: f32 = 64.0;
pub const MAX_JUMPS: f32 = 2.0;
pub const JUMP_VEL: f32 = 64.0;
pub const CLIMB_ANGLE: f32 = FRAC_PI_3 - R_E;
pub const SLIDE_ANGLE: f32 = FRAC_PI_3 - R_E;
pub const HOVER_HEIGHT: f32 = 2.0;
const G1: rapier3d::geometry::Group = rapier3d::geometry::Group::GROUP_1;

pub struct PlayerControllerPlugin;

impl Plugin for PlayerControllerPlugin {
	fn build(&self, app: &mut App) {
		app.add_startup_system(setup)
			.add_system_to_stage(PreUpdate, gravity)
			.add_system_to_stage(PreUpdate, ctrl::repel_ground.after(gravity))
			// .add_system(tick_cooldown::<Jump>)
			.add_system_to_stage(CoreStage::PreUpdate, reset_jump_on_ground)
			.add_system(input::movement_input.before(terminal_velocity))
			.add_system(input::look_input.before(terminal_velocity))
			.add_system(camera::position_target.after(input::look_input))
			.add_system(camera::follow_target.after(camera::position_target))
			.add_system(move_player.after(terminal_velocity))
			.add_system(idle)
			.add_system_to_stage(CoreStage::Last, reset_oob);
	}
}

fn setup(
	mut cmds: Commands,
	mut materials: ResMut<Assets<StandardMaterial>>,
	mut meshes: ResMut<Assets<Mesh>>,
	asset_server: Res<AssetServer>,
) {
	let id = unsafe { PlayerId::new_unchecked(1) };
	spawn_camera(&mut cmds, id);

	let ship = asset_server.load("ships/rocket_baseA.glb#Scene0");
	let vis = SceneBundle {
		scene: ship,
		transform: Transform {
			translation: Vec3::new(-1.875, -0.625, 0.25),
			rotation: Quat::from_rotation_y(FRAC_PI_4),
			scale: Vec3::splat(0.75),
		},
		..default()
	};

	let particle_mesh = Mesh::from(shape::Torus {
		radius: 0.640,
		ring_radius: 0.064,
		subdivisions_segments: 8,
		subdivisions_sides: 3,
	});
	let particle_mesh = meshes.add(particle_mesh);
	let particle_material = materials.add(StandardMaterial {
		// base_color: Color::rgba(0.0, 1.0, 0.5, 0.3),
		base_color: Color::NONE,
		emissive: Color::rgb(0.0, 1.0, 0.6),
		reflectance: 0.0,
		..default()
	});

	let particle_mesh = MaterialMeshBundle {
		mesh: particle_mesh,
		material: particle_material,
		..default()
	};
	cmds.spawn_player(id, vis, particle_mesh);
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
	OrbitalParticle,
}
use player_entity::*;

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
	) -> EntityCommands<'w, 's, 'c>;
}

impl<'c, 'w: 'c, 's: 'c> SpawnPlayer<'c, 'w, 's> for Commands<'w, 's> {
	fn spawn_player(
		&'c mut self,
		id: PlayerId,
		vis: SceneBundle,
		particle_mesh: MaterialMeshBundle<StandardMaterial>,
	) -> EntityCommands<'w, 's, 'c> {
		let owner = BelongsToPlayer::with_id(id);
		let char_collider = Collider::ball(0.8);
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
					CollisionGroups::new(Group::empty(), Group::empty()),
					KinematicCharacterController {
						up: Vect::Z,
						snap_to_ground: None,
						autostep: Some(CharacterAutostep {
							max_height: CharacterLength::Relative(1.15),
							min_width: CharacterLength::Relative(0.5),
							..default()
						}),
						max_slope_climb_angle: CLIMB_ANGLE,
						min_slope_slide_angle: SLIDE_ANGLE,
						filter_groups: Some(InteractionGroups::new(G1, !G1)),
						..default()
					},
					InputManagerBundle::<PlayerAction> {
						input_map: PlayerAction::input_map(),
						..default()
					},
					PlayerAction::abilities_bundle(),
				))
				.set_enum(PlayerEntity::Controller);
		});
		player_vis(&mut root, owner, vis, particle_mesh);
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
			TransformBundle::from_transform(Transform::from_rotation(Quat::from_rotation_x(
				FRAC_PI_2,
			))),
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

					builder.spawn((
						owner,
						SpewerBundle {
							spewer: Spewer {
								factory: Arc::new(move |cmds, xform, time_created| {
									let xform = xform.compute_transform();
									let xform = Transform {
										translation: Vec3 {
											z: xform.translation.z - rand::random::<f32>() * 0.2,
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

pub fn reset_jump_on_ground(
	mut q: Query<(
		AbilityState<PlayerAction>,
		&KinematicCharacterControllerOutput,
	)>,
) {
	for (mut state, out) in &mut q {
		if out.grounded {
			let charges = state.charges.get_mut(PlayerAction::Jump).as_mut().unwrap();
			charges.set_charges(charges.max_charges());
		}
	}
}

pub fn gravity(mut q: Query<(&mut CtrlVel, &KinematicCharacterControllerOutput)>, t: Res<Time>) {
	for (mut ctrl_vel, out) in q.iter_mut() {
		if out.grounded {
			ctrl_vel.linvel.z = 0.0
		}

		let mut info = vec![(TOIStatus::Converged, Vect::NAN, Vect::NAN); 4];
		for (i, col) in out.collisions.iter().enumerate() {
			if let Some(slot) = info.get_mut(i) {
				*slot = (col.toi.status, col.translation_remaining, col.toi.normal1)
			}
		}

		let decr = PLAYER_GRAVITY * t.delta_seconds();

		ctrl_vel.linvel.z -= decr;
	}
}

pub fn move_player(
	mut body_q: Query<(&mut Transform, &BelongsToPlayer), ReadPlayerEntity<Root>>,
	mut vis_q: Query<(&mut Transform, &BelongsToPlayer), ReadPlayerEntity<Vis>>,
	mut ctrl_q: Query<
		(
			&CtrlVel,
			&mut KinematicCharacterController,
			&BelongsToPlayer,
		),
		ReadPlayerEntity<Controller>,
	>,
	t: Res<Time>,
) {
	for (ctrl_vel, mut ctrl, ctrl_owner) in &mut ctrl_q {
		let mut body_xform = body_q
			.iter_mut()
			.find_map(|(xform, owner)| (owner == ctrl_owner).then_some(xform))
			.unwrap();
		let mut vis_xform = vis_q
			.iter_mut()
			.find_map(|(xform, owner)| (owner == ctrl_owner).then_some(xform))
			.unwrap();

		let dt = t.delta_seconds();

		let Vec3 { x, y, z } = ctrl_vel.angvel * dt;
		let rot = Quat::from_euler(EulerRot::ZXY, z, x, y);
		body_xform.rotate_local(rot);

		let slide = body_xform.rotation * (ctrl_vel.linvel * dt);
		let target_tilt = Vec3::new(ctrl_vel.linvel.x * -0.016, -1.0, ctrl_vel.linvel.y * 0.016)
			.normalize_or_zero();
		vis_xform.rotation = Quat::from_rotation_arc(Vec3::NEG_Y, target_tilt);
		ctrl.translation = Some(slide);
	}
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

pub fn idle(mut q: Query<&mut Transform, ReadPlayerEntity<Vis>>, t: Res<Time>) {
	for mut xform in &mut q {
		xform.translation.y = (t.elapsed_seconds() * 3.0).sin() * 0.16;
	}
}
