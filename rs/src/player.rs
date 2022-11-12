use crate::input::{InputVel, PlayerAction};
use crate::{Ability, Despawner, E, input, terminal_velocity, TerminalVelocity, tick_cooldown};
use bevy::{
	core_pipeline::clear_color::ClearColorConfig,
	prelude::{*, CoreStage::PreUpdate},
	ecs::system::EntityCommands,
};
use bevy_rapier3d::{
	control::{KinematicCharacterController, KinematicCharacterControllerOutput},
	dynamics::{CoefficientCombineRule::Min, RigidBody, Velocity},
	geometry::{Collider, Friction},
	math::{Rot, Vect},
	prelude::{*, ColliderMassProperties::Mass},
};
use enum_components::{EntityEnumCommands, EnumComponent};
use leafwing_abilities::prelude::*;
use leafwing_input_manager::prelude::*;
use particles::{
	InitialTransform,
	Lifetime,
	ParticleBundle,
	Spewer,
	SpewerBundle,
	update::{Linear, TargetScale}
};
use rapier3d::{
	control::{CharacterAutostep, CharacterLength},
	prelude::{Isometry, JointAxesMask}
};
use std::{
	f32::consts::*,
	num::NonZeroU8,
	sync::Arc,
	time::Duration
};
use bevy::core_pipeline::bloom::BloomSettings;
use leafwing_input_manager::orientation::Orientation;

pub const MAX_SPEED: f32 = 32.0;
pub const ACCEL: f32 = 64.0;
pub const MAX_JUMPS: f32 = 3.0;
pub const CLIMB_ANGLE: f32 = FRAC_PI_3 - E;
pub const SLIDE_ANGLE: f32 = FRAC_PI_3 - E;

pub struct PlayerControllerPlugin;

impl Plugin for PlayerControllerPlugin {
	fn build(&self, app: &mut App) {
		app.add_startup_system(setup)
			.add_system_to_stage(PreUpdate, gravity)
			// .add_system(tick_cooldown::<Jump>)
			.add_system(reset_jump_on_ground.before(terminal_velocity))
			.add_system(input::movement_input.before(terminal_velocity))
			.add_system(input::look_input.before(terminal_velocity))
			.add_system(position_camera_target.after(input::look_input))
			// .add_system(follow_camera_target.after(position_camera_target))
			.add_system(move_player.after(terminal_velocity))
			.add_system(idle);
	}
}

fn setup(
	mut cmds: Commands,
	mut materials: ResMut<Assets<StandardMaterial>>,
	mut meshes: ResMut<Assets<Mesh>>,
	asset_server: Res<AssetServer>,
) {
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
	let vis_collider = Collider::ball(0.96);

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
		..default()
	});

	let particle_mesh = MaterialMeshBundle {
		mesh: particle_mesh,
		material: particle_material,
		..default()
	};
	let id = unsafe { PlayerId::new_unchecked(1) };
	cmds.spawn_player(id, vis, vis_collider, particle_mesh);
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
		vis_collider: Collider,
		particle_mesh: MaterialMeshBundle<StandardMaterial>,
	) -> EntityCommands<'w, 's, 'c>;
}

#[derive(EnumComponent)]
pub enum PlayerEntity {
	Root,
	Controller,
	Vis,
	VisCollider,
	CamTarget,
	CamPivot,
	Cam,
	HoverParticle { start_xform: Vec3 },
}
use player_entity::*;

const G1: rapier3d::geometry::Group = rapier3d::geometry::Group::GROUP_1;

impl<'c, 'w: 'c, 's: 'c> SpawnPlayer<'c, 'w, 's> for Commands<'w, 's> {
	fn spawn_player(
		&'c mut self,
		id: PlayerId,
		vis: SceneBundle,
		vis_collider: Collider,
		particle_mesh: MaterialMeshBundle<StandardMaterial>,
	) -> EntityCommands<'w, 's, 'c> {
		let owner = BelongsToPlayer::with_id(id);
		let vis_clone = SceneBundle {
			scene: vis.scene.clone(),
			transform: vis.transform.clone(),
			global_transform: vis.global_transform.clone(),
			visibility: vis.visibility.clone(),
			computed_visibility: vis.computed_visibility.clone(),
		};
		let vis_col_clone = vis_collider.clone();
		let particle_mesh_clone = particle_mesh.clone();
		let (char_collider, rot, z_pos) =
			(Collider::round_cone(0.640, 0.48, 0.256), -FRAC_PI_2, 0.640);
		let mut root = self.spawn((
			owner,
			TerminalVelocity(Velocity {
				linvel: Vect::splat(80.0),
				angvel: Vect::new(0.0, 0.0, TAU * 60.0), // one rotation per frame at 60 fps
			}),
			Despawner::new(move |mut cmds: EntityCommands| {
				cmds.commands().spawn_player(
					id,
					SceneBundle {
						scene: vis_clone.scene.clone(),
						transform: vis_clone.transform.clone(),
						global_transform: vis_clone.global_transform.clone(),
						visibility: vis_clone.visibility.clone(),
						computed_visibility: vis_clone.computed_visibility.clone(),
					},
					vis_col_clone.clone(),
					particle_mesh_clone.clone(),
				);
				cmds.despawn_recursive();
			}),
			RigidBody::KinematicPositionBased,
			TransformBundle::default(),
			Velocity::default(),
			Friction {
				coefficient: 0.0,
				combine_rule: Min,
			},
		));
		root.set_enum(PlayerEntity::Root);
		let pivot = root
			.commands()
			.spawn((
				owner,
				CameraVertSlider(0.4),
				TransformBundle::from_transform(Transform {
					translation: Vect::new(0.0, 3.2, 0.384),
					..default()
				}),
			))
			.set_enum(PlayerEntity::CamPivot)
			.id();
		let camera = (
			owner,
			Camera3dBundle {
				camera: Camera {
					// TODO: This causes crashes in debug, default settings are way too exaggerated in release,
					//   it causes performance hits, and results don't look quite right even when it does work
					#[cfg(all(not(debug_assertions), not(target_arch = "wasm32")))]hdr: true,
					..default()
				},
				transform: Transform {
					rotation: Quat::from_rotation_x(FRAC_PI_2),
					..default()
				},
				camera_3d: Camera3d {
					clear_color: ClearColorConfig::Custom(Color::rgb(0.1, 0.0, 0.15)),
					..default()
				},
				..default()
			},
			BloomSettings {
				intensity: 0.003,
				..default()
			},
			Despawner::new(|_| {
				// Don't want camera disappearing. Maybe try to reset whole player to origin?
			}),
			Pivot(pivot),
		);
		let mut camera = root.commands().spawn(camera).set_enum(PlayerEntity::Cam).id();
		let mut cam_target = root.commands().spawn((
			owner,
			TransformBundle::default(),
			Collider::ball(0.72),
			CollisionGroups::new(Group::empty(), Group::empty()),
		));
		cam_target.set_enum(PlayerEntity::CamTarget);
		cam_target.add_child(camera);
		
		root.with_children(|builder| {
			builder
				.spawn((
					owner,
					char_collider,
					TransformBundle::from(Transform {
						translation: Vec3::Z * z_pos,
						rotation: Quat::from_rotation_x(rot),
						..default()
					}),
					InputVel::default(),
					CollisionGroups::new(Group::empty(), Group::empty()),
					KinematicCharacterController {
						up: Vect::Z,
						snap_to_ground: Some(CharacterLength::Relative(0.5)),
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
		player_vis(&mut root, owner, vis, vis_collider, particle_mesh, pivot);
		root
	}
}

fn player_vis(
	cmds: &mut EntityCommands,
	owner: BelongsToPlayer,
	vis: SceneBundle,
	vis_collider: Collider,
	particle_mesh: MaterialMeshBundle<StandardMaterial>,
	cam_pivot: Entity,
) {
	use rapier3d::prelude::{JointAxis::*, MotorModel::ForceBased};
	let anchor = cmds.id();
	let mut joint = GenericJoint::new(JointAxesMask::FREE_SPHERICAL_AXES);
	joint
		.set_local_anchor2(Vect::new(0.0, -1.536, 0.0))
		.set_local_basis2(Rot::from_rotation_x(-FRAC_PI_2))
		.set_motor_model(X, ForceBased)
		.set_motor_model(Y, ForceBased)
		.set_motor_model(Z, ForceBased)
		.set_motor(X, 0.0, 0.0, 5.12, 0.096)
		.set_motor(Y, 0.0, 0.0, 5.12, 0.096)
		.set_motor(Z, 0.0, 0.0, 2.56, 0.032);

	cmds.commands()
		.spawn((
			owner,
			RigidBody::Dynamic,
			CollisionGroups::new(Group::GROUP_1, !Group::GROUP_1),
			vis_collider,
			ImpulseJoint::new(anchor, joint),
			Sleeping {
				linear_threshold: -1.0,
				angular_threshold: -1.0,
				sleeping: false,
			},
			Mass(0.01),
			// Restitution::new(1.0),
			TransformBundle::default(),
			VisibilityBundle::default(), // for children ComputedVisibility
		))
		.set_enum(PlayerEntity::VisCollider)
		.with_children(move |builder| {
			// Mesh is a child so we can apply transform independent of collider to align them
			builder
				.spawn((owner, TransformBundle::default(), VisibilityBundle::default()))
				.set_enum(PlayerEntity::Vis)
				.with_children(|builder| {
					builder.spawn((owner, vis));
					let transform = Transform {
						rotation: Quat::from_rotation_x(FRAC_PI_2),
						scale: Vec3::new(1.0, 0.0, 1.0),
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
		.add_child(cam_pivot);
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

pub fn gravity(mut q: Query<(&mut InputVel, &KinematicCharacterControllerOutput)>, t: Res<Time>) {
	for (mut input_vel, out) in q.iter_mut() {

		if out.grounded {
			input_vel.linvel.z = 0.0
		}

		let mut info = vec![(TOIStatus::Converged, Vect::NAN, Vect::NAN); 4];
		for (i, col) in out.collisions.iter().enumerate() {
			if let Some(slot) = info.get_mut(i) {
				*slot = (col.toi.status, col.translation_remaining, col.toi.normal1)
			}
		}

		let decr = ACCEL * t.delta_seconds();

		input_vel.linvel.z -= decr;
	}
}

pub fn move_player(
	mut body_q: Query<&mut Transform, ReadPlayerEntity<Root>>,
	mut vis_q: Query<&mut Transform, ReadPlayerEntity<Vis>>,
	mut ctrl_q: Query<(
		&InputVel,
		&mut KinematicCharacterController,
	)>,
	t: Res<Time>,
) {
	let mut xform = body_q.single_mut();
	let (input_vel, mut ctrl) = ctrl_q.single_mut();

	let dt = t.delta_seconds();
	
	let Vec3 { x, y, z } = input_vel.angvel * dt;
	let rot = Quat::from_euler(EulerRot::ZXY, z, x, y);
	xform.rotate_local(rot);

	let slide = xform.rotation * (input_vel.linvel * dt);
	let mut vis_xform = vis_q.single_mut();
	let target_tilt =
		Vec3::new(input_vel.linvel.x * -0.016, -1.0, input_vel.linvel.y * 0.016).normalize();
	vis_xform.rotation = Quat::from_rotation_arc(Vec3::NEG_Y, target_tilt);
	ctrl.translation = Some(slide.into());
}

#[derive(Component, Debug, Default, Copy, Clone, Deref, DerefMut)]
pub struct TargetIso(Isometry<Real>);

#[derive(Component, Debug, Copy, Clone, Deref, DerefMut, Reflect)]
pub struct Pivot(Entity);

#[derive(Component, Debug, Default, Copy, Clone, Reflect)]
pub struct CameraVertSlider(pub f32);

const MAX_CAM_DIST: f32 = 20.0;
const MIN_CAM_DIST: f32 = 3.2;

fn position_camera_target(
	ctx: Res<RapierContext>,
	cam_pivot_q: Query<(&GlobalTransform, &BelongsToPlayer), ReadPlayerEntity<CamPivot>>,
	mut cam_target_q: Query<(&mut Transform, &Collider, &BelongsToPlayer), ReadPlayerEntity<CamTarget>>,
	mut cam_q: Query<(&Pivot, &BelongsToPlayer), ReadPlayerEntity<Cam>>,
) {
	let Ok((mut target_xform, col, target_owner)) = cam_target_q.get_single_mut() else {
		return;
	};
	for (pivot, cam_owner) in &mut cam_q {
		let (pivot_xform, pivot_owner) = cam_pivot_q.get(**pivot).unwrap();
		let filter = QueryFilter::from(InteractionGroups::new(G1, !G1));
		let (_, rot, tr) = pivot_xform.to_scale_rotation_translation();
		let dir = rot * Vect::NEG_Y;
		let pos = tr + (dir * MIN_CAM_DIST); // start at minimum distance, not player origin
		let result = ctx.cast_shape(pos, -rot, dir, col, MAX_CAM_DIST - MIN_CAM_DIST, filter);
		let toi = if let Some(result) = result {
			// screen_print!("{result:#?}");
			let toi = result.1.toi;
			if toi == 0.0 {
				if ctx
					.cast_shape(
						tr + dir * MAX_CAM_DIST,
						-rot,
						-dir,
						col,
						(MAX_CAM_DIST - MIN_CAM_DIST) * 0.4, // Don't want enormous object right in front of camera
						filter,
					)
					.is_some()
				{
					// Max distance is still inside something, just let it be close
					toi
				} else {
					// Something is in-between character and camera, but better than camera being inside it
					MAX_CAM_DIST
				}
			} else {
				toi
			}
		} else {
			MAX_CAM_DIST
		};
		target_xform.translation = (tr + dir * (toi + MIN_CAM_DIST)).into();
		target_xform.rotation = -rot;
		// screen_print!("{target_xform:#?}");
	}
}

fn follow_camera_target(
	mut cam_q: Query<(&mut Transform, &BelongsToPlayer), ReadPlayerEntity<Cam>>,
	pivot_q: Query<(&GlobalTransform, &BelongsToPlayer), ReadPlayerEntity<CamPivot>>,
	target_q: Query<(&GlobalTransform, &BelongsToPlayer), ReadPlayerEntity<CamTarget>>,
) {
	for (mut cam_xform, cam_owner) in &mut cam_q {
		let pivot_xform = pivot_q.iter().find_map(|(xform, owner)|
			(owner == cam_owner).then(|| *xform)
		).unwrap();
		let target_xform = target_q.iter().find_map(|(xform, owner)|
			(owner == cam_owner).then(|| *xform)
		).unwrap();
		*cam_xform = target_xform.compute_transform();
		cam_xform.rotate(Quat::from_rotation_x(FRAC_PI_2));
	}
}

pub fn idle(mut q: Query<&mut Transform, ReadPlayerEntity<Vis>>, t: Res<Time>) {
	for mut xform in &mut q {
		xform.translation.y = (t.elapsed_seconds() * 3.0).sin() * 0.16;
	}
}
