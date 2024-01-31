use crate::{terminal_velocity, NeverDespawn, TerminalVelocity};

use crate::{pickups::MissSfx, settings::Settings, util::IntoFnPlugin};
use bevy::{
	ecs::system::EntityCommands,
	prelude::{
		shape::{Icosphere, RegularPolygon},
		*,
	},
	render::camera::ManualTextureViews,
	utils::{HashMap, HashSet},
};
use bevy_common_assets::ron::RonAssetPlugin;
use bevy_kira_audio::{Audio, AudioControl};
use bevy_rapier3d::{
	dynamics::{CoefficientCombineRule::Min, Velocity},
	geometry::{Collider, Friction},
	math::Vect,
	parry::math::Isometry,
	plugin::PhysicsSet::StepSimulation,
	prelude::{RigidBody::KinematicPositionBased, *},
};
use camera::spawn_camera;
use ctrl::{CtrlState, CtrlVel};
use enum_components::{ERef, EntityEnumCommands, EnumComponent};
use input::PlayerAction;
use leafwing_input_manager::prelude::*;
use nanorand::Rng;
use particles::{
	update::{Linear, TargetScale},
	InitialGlobalTransform, InitialTransform, Lifetime, ParticleBundle, PreviousGlobalTransform,
	PreviousTransform, Spewer, SpewerBundle,
};
use prefs::PlayerPrefs;
use rapier3d::{
	math::Point,
	prelude::{Aabb, SharedShape},
};
use std::{f32::consts::*, num::NonZeroU8, ops::Add, time::Duration};

pub mod camera;
pub mod ctrl;
pub mod input;
pub mod prefs;
pub mod tune;

const G1: Group = Group::GROUP_1;

#[derive(SystemSet, Debug, Hash, PartialEq, Eq, Clone)]
pub enum InterpolatedXforms {
	Propagate,
	Sync,
}

pub fn plugin(app: &mut App) -> &mut App {
	app.add_plugins((
		input::plugin.plugfn(),
		RonAssetPlugin::<PlayerParams>::new(&["ron"]),
		crate::anim::AnimationPlugin::<RotVel>::default(),
	))
	.insert_resource(PlayerRespawnTimers::default())
	.add_systems(Startup, setup)
	.add_systems(
		First,
		(
			Prev::<CtrlState>::update_component,
			tune::extract_loaded_params,
		),
	)
	.add_systems(
		Update,
		(
			ctrl::gravity
				.ambiguous_with(input::InputSystems) // Gravity only affects z, input only affects xy
				.before(terminal_velocity)
				.run_if(resource_exists::<PlayerParams>()),
			camera::position_target.after(terminal_velocity),
			camera::follow_target.after(camera::position_target),
			ctrl::reset_jump_on_ground
				.before(input::InputSystems)
				.run_if(resource_exists::<PlayerParams>()),
			ctrl::move_player
				.before(StepSimulation)
				.after(terminal_velocity)
				.after(input::InputSystems)
				.run_if(resource_exists::<PlayerParams>()),
			idle,
			orbs_follow_arms,
		),
	)
	.add_systems(
		Last,
		(
			reset_oob.before(crate::despawn_oob),
			kill_on_key,
			countdown_respawn,
			play_death_sound.after(kill_on_key).after(reset_oob),
			spawn_players
				.after(countdown_respawn)
				.run_if(resource_exists::<PlayerParams>()),
		),
	)
	.add_event::<PlayerSpawnEvent>();
	app
}

pub fn setup(
	mut cmds: Commands,
	mut materials: ResMut<Assets<StandardMaterial>>,
	mut meshes: ResMut<Assets<Mesh>>,
	mut images: ResMut<Assets<Image>>,
	asset_server: Res<AssetServer>,
	settings: Res<Settings>,
	mut spawn_events: EventWriter<PlayerSpawnEvent>,
	manual_texture_views: Res<ManualTextureViews>,
) {
	Box::leak(Box::new(
		asset_server.load::<PlayerParams>("tune/player_params.ron"),
	));

	cmds.insert_resource(PlayerBounds {
		aabb: Aabb::new(
			Point::new(-16384.0, -16384.0, -8192.0),
			Point::new(16384.0, 16384.0, 8192.0),
		),
	});

	let id = unsafe { PlayerId::new_unchecked(1) };
	spawn_camera(
		&mut cmds,
		id,
		&settings,
		&mut images,
		&asset_server,
		&manual_texture_views,
	);

	let aoe_sfx = asset_server.load("sfx/SFX_-_magic_spell_03.ogg");
	cmds.insert_resource(Sfx {
		aoe: aoe_sfx,
		fire_a: Handle::default(),
	});

	let ship_scene = asset_server.load("ships/player.glb#Scene0");

	let antigrav_pulse_mesh = Mesh::from(shape::Torus {
		radius: 0.640,
		ring_radius: 0.064,
		subdivisions_segments: 6,
		subdivisions_sides: 3,
	});
	let antigrav_pulse_mesh = meshes.add(antigrav_pulse_mesh);
	let antigrav_pulse_mat = materials.add(StandardMaterial {
		// base_color: Color::rgba(0.0, 1.0, 0.5, 0.3),
		base_color: Color::NONE,
		emissive: Color::rgb(0.0, 12.0, 7.2),
		reflectance: 0.0,
		..default()
	});

	let arm_mesh = Mesh::try_from(Icosphere {
		radius: 0.3,
		subdivisions: 2,
	})
	.expect("create icosphere mesh");
	let arm_mesh = meshes.add(arm_mesh);
	let arm_particle_mesh = meshes.add(Mesh::from(RegularPolygon::new(0.1, 3)));
	let arm_mat_template = StandardMaterial {
		base_color: Color::NONE,
		reflectance: 0.0,
		double_sided: true,
		cull_mode: None,
		..default()
	};
	let arm_mats = [
		materials.add(StandardMaterial {
			emissive: Color::GREEN * 6.0,
			..arm_mat_template.clone()
		}),
		materials.add(StandardMaterial {
			emissive: Color::WHITE * 3.0,
			..arm_mat_template.clone()
		}),
		materials.add(StandardMaterial {
			emissive: Color::CYAN * 6.0,
			..arm_mat_template
		}),
	];

	let crosshair_mesh = meshes.add(
		shape::Torus {
			radius: 0.5,
			ring_radius: 0.1,
			..default()
		}
		.into(),
	);

	let crosshair_mat = materials.add(StandardMaterial {
		base_color: Color::YELLOW.into(),
		depth_bias: f32::INFINITY,
		unlit: true,
		..default()
	});

	cmds.insert_resource(PlayerSpawnData {
		ship_scene,
		arm_mesh,
		antigrav_pulse_mesh,
		antigrav_pulse_mat,
		arm_particle_mesh,
		arm_mats,
		crosshair_mesh,
		crosshair_mat,
	});

	spawn_events.send(PlayerSpawnEvent { id });
}

#[derive(Resource, Clone, Debug)]
pub struct PlayerSpawnData {
	pub ship_scene: Handle<Scene>,
	pub antigrav_pulse_mesh: Handle<Mesh>,
	pub antigrav_pulse_mat: Handle<StandardMaterial>,
	pub arm_mesh: Handle<Mesh>,
	pub arm_particle_mesh: Handle<Mesh>,
	pub arm_mats: [Handle<StandardMaterial>; 3],
	pub crosshair_mesh: Handle<Mesh>,
	pub crosshair_mat: Handle<StandardMaterial>,
}

#[derive(EnumComponent)]
#[component(mutable, derive(Debug, PartialEq, Eq))]
pub enum PlayerEntity {
	Root,
	Controller,
	Ship,
	ShipCenter,
	CamPivot,
	Cam,
	Crosshair,
	AntigravParticles,
	Arms,
	Arm(PlayerArm),
	Orb(PlayerArm),
}
use crate::{
	abilities::{BoosterCharge, HurtboxFilter, Sfx, WeaponCharge},
	anim::ComponentDelta,
	player::tune::PlayerParams,
	util::{Diff, Prev, TransformDelta},
};
use player_entity::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum PlayerArm {
	A,
	B,
	C,
}

pub type PlayerId = NonZeroU8;

#[derive(Component, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Deref)]
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

pub fn spawn_players(
	mut cmds: Commands,
	spawn_data: Res<PlayerSpawnData>,
	params: Res<PlayerParams>,
	mut events: ResMut<Events<PlayerSpawnEvent>>,
) {
	for event in events.drain() {
		let PlayerSpawnData {
			ship_scene,
			antigrav_pulse_mesh,
			antigrav_pulse_mat,
			arm_mesh,
			arm_particle_mesh,
			arm_mats: [mat_a, mat_b, mat_c],
			crosshair_mesh,
			crosshair_mat,
		} = spawn_data.clone();

		let vis = SceneBundle {
			scene: ship_scene,
			..default()
		};

		let antigrav_pulse_mesh = MaterialMeshBundle {
			mesh: antigrav_pulse_mesh,
			material: antigrav_pulse_mat,
			..default()
		};

		let arm1 = MaterialMeshBundle::<StandardMaterial> {
			mesh: arm_mesh.clone(),
			material: mat_a,
			transform: Transform::from_translation(Vec3::X * 2.0),
			..default()
		};
		let arm2 = MaterialMeshBundle::<StandardMaterial> {
			mesh: arm_mesh.clone(),
			material: mat_b,
			transform: Transform::from_translation(
				Quat::from_rotation_z(FRAC_PI_3 * 2.0) * Vec3::X * 2.0,
			),
			..default()
		};
		let arm3 = MaterialMeshBundle::<StandardMaterial> {
			mesh: arm_mesh,
			material: mat_c,
			transform: Transform::from_translation(
				Quat::from_rotation_z(FRAC_PI_3 * 4.0) * Vec3::X * 2.0,
			),
			..default()
		};

		let id = event.id;
		let owner = BelongsToPlayer::with_id(id);
		let char_collider = Collider::from(SharedShape::new(params.phys.collider));
		let mut root = cmds
			.spawn((
				Name::new(format!("Player{}", owner.0.get())),
				owner,
				TerminalVelocity(Velocity {
					linvel: Vect::splat(96.0),
					angvel: Vect::new(0.0, 0.0, PI / crate::DT), // Half a rotation per physics tick
				}),
				KinematicPositionBased,
				TransformBundle::default(),
				Velocity::default(),
				Friction {
					coefficient: 0.0,
					combine_rule: Min,
				},
				VisibilityBundle::default(),
			))
			.with_enum(Root);

		build_player_scene(
			&mut root,
			owner,
			vis,
			char_collider,
			antigrav_pulse_mesh,
			[
				(arm1, PlayerArm::A),
				(arm2, PlayerArm::B),
				(arm3, PlayerArm::C),
			],
			arm_particle_mesh,
			(crosshair_mesh, crosshair_mat),
		);
	}
}

fn build_player_scene(
	root: &mut EntityCommands,
	owner: BelongsToPlayer,
	vis: SceneBundle,
	char_collider: Collider,
	particle_mesh: MaterialMeshBundle<StandardMaterial>,
	arm_meshes: [(MaterialMeshBundle<StandardMaterial>, PlayerArm); 3],
	arm_particle_mesh: Handle<Mesh>,
	crosshair: (Handle<Mesh>, Handle<StandardMaterial>),
) {
	player_controller(root, owner, char_collider);
	player_vis(
		root,
		owner,
		vis,
		particle_mesh,
		arm_meshes,
		arm_particle_mesh,
		crosshair,
	);
}

fn player_controller(root: &mut EntityCommands, owner: BelongsToPlayer, char_collider: Collider) {
	root.with_children(|builder| {
		let PlayerPrefs {
			invert_camera,
			fov,
			input_map,
			sens,
		} = PlayerPrefs::default();
		builder
			.spawn((
				Name::new(format!("Player{}.Controller", owner.0.get())),
				owner,
				char_collider,
				Restitution::new(0.5),
				Ccd::enabled(),
				TransformBundle::default(),
				CtrlVel::default(),
				CollisionGroups::new(Group::GROUP_1, !Group::GROUP_1),
				InputManagerBundle::<PlayerAction> {
					input_map,
					..default()
				},
				CtrlState::default(),
				BoosterCharge::default(),
				WeaponCharge::default(),
				(invert_camera, fov, sens),
			))
			.with_enum(Controller);
	});
}

fn player_vis(
	root: &mut EntityCommands,
	owner: BelongsToPlayer,
	vis: SceneBundle,
	particle_mesh: MaterialMeshBundle<StandardMaterial>,
	arm_meshes: [(MaterialMeshBundle<StandardMaterial>, PlayerArm); 3],
	arm_particle_mesh: Handle<Mesh>,
	crosshair: (Handle<Mesh>, Handle<StandardMaterial>),
) {
	let camera_pivot = camera::spawn_pivot(root.commands(), owner, crosshair).id();
	let root_id = root.id();
	let ship_center = root
		.commands()
		.spawn((
			Name::new(format!("Player{}.ShipCenter", owner.0.get())),
			owner,
			TransformBundle::from_transform(Transform::from_translation(Vec3::NEG_Z * 0.64)),
			TransformInterpolation {
				start: None,
				end: Some(Isometry::translation(0.0, 0.0, -0.64)),
			},
			VisibilityBundle::default(), // for children ComputedVisibility
		))
		.set_enum(ShipCenter)
		.with_children(move |builder| {
			// Mesh is a child so we can apply transform independent of collider to align them
			builder
				.spawn((
					Name::new(format!("Player{}.ShipCenter.Ship", owner.0.get())),
					owner,
					TransformBundle::from_transform(Transform::from_rotation(
						Quat::from_rotation_x(FRAC_PI_2),
					)),
					VisibilityBundle::default(),
				))
				.set_enum(Ship)
				.with_children(|builder| {
					builder.spawn((
						Name::new(format!("Player{}.ShipCenter.Ship.Vis", owner.0.get())),
						owner,
						vis,
					));

					let transform = Transform {
						// rotation: Quat::from_rotation_x(FRAC_PI_2),
						..default()
					};
					let particle_mesh = MaterialMeshBundle {
						transform,
						..particle_mesh
					};

					let mut rng = nanorand::WyRand::new();
					builder
						.spawn((
							Name::new(format!(
								"Player{}.ShipCenter.Ship.AntigravParticles",
								owner.0.get()
							)),
							owner,
							SpewerBundle {
								spewer: Spewer {
									factory: Box::new(move |cmds, xform, time_created| {
										let xform = xform.compute_transform();
										let scale_rng = rng.generate::<f32>() * 0.6 + 0.6;
										let xform = Transform {
											translation: Vec3 {
												z: xform.translation.z
													- rng.generate::<f32>() * 0.2,
												..xform.translation
											},
											scale: xform.scale * scale_rng,
											..xform
										};
										cmds.spawn((
											ParticleBundle {
												mesh_bundle: MaterialMeshBundle {
													transform: xform,
													..particle_mesh.clone()
												},
												lifetime: Lifetime(Duration::from_secs_f32(0.24)),
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
									interval: Duration::from_secs_f32(0.072),
									// jitter: Duration::from_secs_f32(0.033),
									use_global_coords: true,
									..default()
								},
								transform: TransformBundle::from_transform(Transform {
									translation: Vec3::new(0.0, -0.5, 0.0),
									..default()
								}),
								..default()
							},
						))
						.set_enum(AntigravParticles);

					builder.spawn((
						Name::new(format!("Player{}.ShipCenter.Ship.Glow", owner.0.get())),
						PointLightBundle {
							point_light: PointLight {
								color: Color::rgb(0.0, 1.0, 0.6),
								intensity: 2048.0,
								range: 12.0,
								shadows_enabled: false,
								..default()
							},
							transform: Transform::from_xyz(0.0, -0.5, 0.0),
							..default()
						},
					));
				});
		})
		.with_children(|builder| {
			let mut arms = builder
				.spawn((
					owner,
					Name::new(format!("Player{}.ShipCenter.Arms", owner.0.get())),
					TransformBundle::from_transform(Transform::from_translation(Vec3::Z * 0.64)),
					VisibilityBundle::default(),
					RotVel::new(8.0),
				))
				.with_enum(Arms);
			player_arms(root_id, &mut arms, owner, arm_meshes, arm_particle_mesh);
		})
		.add_child(camera_pivot)
		.id();
	root.add_child(ship_center);
}

#[derive(Component, Debug, Default, Copy, Clone, PartialEq, Reflect, Deref, DerefMut)]
pub struct RotVel {
	// TODO: Maybe quiescent should be a newtype component instead of a field
	pub quiescent: f32,
	#[deref]
	pub current: f32,
}

impl RotVel {
	fn new(vel: f32) -> Self {
		Self {
			quiescent: vel,
			current: vel,
		}
	}

	pub fn with_current(self, current: f32) -> Self {
		Self {
			current,
			quiescent: self.quiescent,
		}
	}

	pub fn quiescent(self) -> Self {
		Self {
			current: self.quiescent,
			quiescent: self.quiescent,
		}
	}
}

impl Diff for RotVel {
	type Delta = f32;

	fn delta_from(&self, rhs: &Self) -> Self::Delta {
		self.current - rhs.current
	}
}

// TODO: derive macro for lerp
impl Add<f32> for RotVel {
	type Output = RotVel;

	fn add(mut self, rhs: f32) -> Self::Output {
		*self += rhs;
		self
	}
}

fn player_arms(
	root_id: Entity,
	arms_pivot: &mut EntityCommands,
	owner: BelongsToPlayer,
	meshes: [(MaterialMeshBundle<StandardMaterial>, PlayerArm); 3],
	particle_mesh: Handle<Mesh>,
) {
	arms_pivot.with_children(|builder| {
		for (arm, which) in &meshes {
			builder
				.spawn((
					owner,
					arm.transform,
					arm.global_transform,
					arm.visibility,
					arm.view_visibility,
					arm.inherited_visibility,
				))
				.with_enum(Arm(*which));
		}
	});
	for (arm, which) in meshes {
		let particle_mesh = particle_mesh.clone();
		let particle_mat = arm.material.clone();
		let mut rng = nanorand::WyRand::new();
		let spewer = Spewer {
			factory: Box::new(move |cmds, glob_xform: &GlobalTransform, time_created| {
				let mut xform = glob_xform.compute_transform();
				xform.translation.x += (rng.generate::<f32>() * 0.42 - 0.21) * xform.scale.x;
				xform.translation.y += (rng.generate::<f32>() * 0.42 - 0.21) * xform.scale.y;
				xform.translation.z += (rng.generate::<f32>() * 0.42 - 0.21) * xform.scale.z;
				let mut xform = Transform {
					scale: Vec3::ONE,
					..xform
				};

				let init_rot_vec = Vec3::new(
					rng.generate::<f32>() * 2.0 - 1.0,
					rng.generate::<f32>() * 2.0 - 1.0,
					rng.generate::<f32>() * 2.0 - 1.0,
				)
				.normalize();
				xform.rotation = Quat::from_rotation_arc(Vec3::Z, init_rot_vec);
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
						initial_global_transform: InitialGlobalTransform(*glob_xform),
						lifetime: Lifetime(Duration::from_secs_f32(0.12)),
					},
					Linear {
						velocity: Vec3::NEG_Z * 2.4,
					},
					TargetScale { scale: Vec3::ZERO },
				))
			}),
			interval: Duration::from_micros(1500),
			use_global_coords: true,
			..default()
		};
		let mut orb = arms_pivot
			.commands()
			.spawn((
				Name::new(format!("Player{}.ShipCenter.Arms.{which:?}", owner.0.get())),
				owner,
				arm,
				spewer,
				PreviousTransform::default(),
				PreviousGlobalTransform::default(),
				Collider::ball(0.4),
				Sensor,
			))
			.with_enum(Orb(which));
		let orb_id = orb.id();
		orb.insert(HurtboxFilter {
			exclude_collider: Some(orb_id),
			exclude_rigid_body: Some(root_id),
			..default()
		});
	}
}

pub fn reset_oob(
	mut cmds: Commands,
	q: Query<(&GlobalTransform, &BelongsToPlayer)>,
	player_nodes: Query<(Entity, &BelongsToPlayer), (Without<NeverDespawn>, Without<Parent>)>,
	bounds: Res<PlayerBounds>,
	mut respawn_timers: ResMut<PlayerRespawnTimers>,
) {
	let mut to_respawn = HashSet::new();
	for (xform, owner) in &q {
		if !bounds
			.aabb
			.contains_local_point(&xform.translation().into())
		{
			bevy::log::error!("Player {owner:?} is out of bounds. Respawning.");
			to_respawn.insert(owner);
		}
	}
	for (id, owner) in &player_nodes {
		if to_respawn.remove(&owner) {
			cmds.entity(id).despawn_recursive();
			respawn_timers.start(**owner, Duration::from_secs(3)).ok();
		}
	}
}

#[derive(Resource, Debug, Deref, DerefMut)]
pub struct PlayerBounds {
	pub aabb: Aabb,
}

pub fn idle(
	mut vis_q: Query<&mut Transform, ERef<Ship>>,
	mut arms_q: Query<(&mut Transform, &RotVel), ERef<Arms>>,
	mut arm_q: Query<&mut Transform, ERef<Arm>>,
	t: Res<Time>,
) {
	let s = t.elapsed_seconds_wrapped();
	for mut xform in &mut vis_q {
		xform.translation.y = (s * 3.0).sin() * 0.24;
		xform.rotate_local_y(-2.0 * t.delta_seconds());
	}
	for (mut xform, rvel) in &mut arms_q {
		xform.rotation =
			(xform.rotation * Quat::from_rotation_z(t.delta_seconds() * **rvel)).normalize();
	}
	for (n, mut xform) in arm_q.iter_mut().enumerate() {
		let phase = (n as f32) * FRAC_PI_3 * 2.0;
		xform.translation = Vec3 {
			z: ((s * 15.1) + phase).sin() * 0.3
				+ ((s * 15.3) + phase).sin() * 0.3
				+ ((s * 15.7) + phase).sin() * 0.3,
			..xform.translation
		};
	}
}

pub fn orbs_follow_arms(
	arm_q: Query<(&GlobalTransform, &BelongsToPlayer, ERef<Arm>)>,
	mut orb_q: Query<(Entity, &BelongsToPlayer, ERef<Orb>)>,
	mut sender: EventWriter<ComponentDelta<Transform>>,
) {
	for (id, owner, which) in &mut orb_q {
		let Some(arm_global) = arm_q.iter().find_map(|(global, arm_owner, which_arm)| {
			(arm_owner == owner && which_arm.0 == which.0).then_some(*global)
		}) else {
			continue;
		};
		let arm_global = arm_global.compute_transform();
		// Always 0 progress to act as default only when no other animations are running
		sender.send(ComponentDelta::<Transform>::new(
			id,
			0.0,
			move |mut val, coef| {
				let diff = arm_global.delta_from(&*val) * coef;
				if diff != TransformDelta::ZERO {
					*val = *val + diff;
				}
			},
		))
	}
}

#[derive(Event, Clone, Debug)]
pub struct PlayerSpawnEvent {
	id: PlayerId,
}

pub fn countdown_respawn(
	mut spawn_events: EventWriter<PlayerSpawnEvent>,
	mut timers: ResMut<PlayerRespawnTimers>,
	t: Res<Time>,
) {
	for (&id, timer) in timers.iter_mut() {
		timer.tick(t.delta());
		if timer.just_finished() {
			spawn_events.send(PlayerSpawnEvent { id });
		}
	}
}

pub fn kill_on_key(
	mut cmds: Commands,
	q: Query<(Entity, &BelongsToPlayer), (Without<NeverDespawn>, Without<Parent>)>,
	input: Res<Input<KeyCode>>,
	mut respawn_timers: ResMut<PlayerRespawnTimers>,
) {
	if input.just_pressed(KeyCode::K) {
		for (id, owner) in &q {
			cmds.entity(id).despawn_recursive();
			respawn_timers.start(**owner, Duration::from_secs(2)).ok();
		}
	}
}

pub fn play_death_sound(
	sound: Res<MissSfx>,
	audio: Res<Audio>,
	respawn_timers: Res<PlayerRespawnTimers>,
) {
	for timer in respawn_timers.values() {
		if timer.elapsed() == Duration::ZERO {
			audio.play(sound.0.clone()).with_volume(0.4);
		}
	}
}

#[derive(Resource, Debug, Default, Deref, DerefMut)]
pub struct PlayerRespawnTimers(HashMap<PlayerId, Timer>);

impl PlayerRespawnTimers {
	pub fn start(&mut self, player: PlayerId, duration: Duration) -> Result<(), Duration> {
		let timer = self
			.0
			.entry(player)
			.or_insert_with(|| Timer::new(duration, TimerMode::Once));
		if timer.finished() {
			timer.set_duration(duration);
			timer.reset();
			Ok(())
		} else {
			Err(timer.remaining())
		}
	}
}
