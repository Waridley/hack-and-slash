use std::{f32::consts::*, num::NonZeroU8, ops::Add, time::Duration};

use bevy::{
	asset::AssetPath,
	ecs::system::EntityCommands,
	input::common_conditions::input_toggle_active,
	prelude::*,
	render::{
		mesh::{SphereKind, SphereMeshBuilder, TorusMeshBuilder},
		primitives::Sphere,
		view::{Layer, RenderLayers},
	},
	utils::{HashMap, HashSet},
};
use bevy_kira_audio::{Audio, AudioControl};
use bevy_pkv::PkvStore;
use bevy_rapier3d::{
	dynamics::CoefficientCombineRule::Min,
	parry::math::Isometry,
	plugin::PhysicsSet::StepSimulation,
	prelude::{RigidBody::KinematicPositionBased, *},
};
use enum_components::{ERef, EntityEnumCommands, EnumComponent, WithVariant};
use leafwing_input_manager::prelude::*;
use nanorand::Rng;
use particles::{
	update::{Linear, TargetScale},
	InitialGlobalTransform, InitialTransform, Lifetime, ParticleBundle, PreviousGlobalTransform,
	PreviousTransform, Spewer, SpewerBundle,
};
use rapier3d::{math::Point, prelude::Aabb};

use camera::spawn_cameras;
use ctrl::{CtrlState, CtrlVel};
use engine::{
	planet::terrain::NeedsTerrain,
	ui::{layout::LineUpChildren, widgets::WidgetGizmos, UiRoot, GLOBAL_UI_LAYER},
};
use player_entity::*;
use prefs::PlayerPrefs;

use crate::{
	anim::ComponentDelta,
	pickups::MissSfx,
	planet::{chunks::ChunkFinder, frame::Frame, PlanetVec2},
	player::{
		abilities::{BoosterCharge, HurtboxFilter, Sfx, WeaponCharge},
		tune::{AbilityParams, PlayerParams, PlayerPhysicsParams},
	},
	settings::Settings,
	terminal_velocity,
	util::{Diff, IntoFnPlugin, Prev, TransformDelta},
	NeverDespawn, TerminalVelocity,
};

pub mod abilities;
pub mod camera;
pub mod ctrl;
pub mod input;
pub mod prefs;
pub mod tune;

const G1: Group = Group::GROUP_1;
pub const fn player_ui_layer(player: PlayerId) -> Layer {
	GLOBAL_UI_LAYER - (player.get() * 2) + 1
}
pub const fn player_hud_layer(player: PlayerId) -> Layer {
	GLOBAL_UI_LAYER - (player.get() * 2)
}

pub fn plugin(app: &mut App) -> &mut App {
	#[cfg(feature = "debugging")]
	{
		macro_rules! init_gizmos {
			($i:literal) => {
				let layers = RenderLayers::layer(player_ui_layer(PlayerId::new($i).unwrap()));
				app.add_systems(
					PostUpdate,
					engine::ui::widgets::draw_widget_shape_gizmos::<$i>
						.run_if(input_toggle_active(false, KeyCode::KeyG))
						.after(bevy::render::view::VisibilitySystems::CheckVisibility),
				)
				.insert_gizmo_group(
					WidgetGizmos::<$i>,
					GizmoConfig {
						render_layers: layers,
						..default()
					},
				);
			};
		}
		init_gizmos!(1);
		init_gizmos!(2);
		init_gizmos!(3);
		init_gizmos!(4);
	}

	app.add_plugins((
		prefs::PrefsPlugin,
		input::plugin.plugfn(),
		crate::anim::AnimationPlugin::<RotVel>::PLUGIN,
	))
	.register_type::<AssetPath>()
	.register_type::<ReflectTorus>()
	.register_type::<ReflectIcosphere>()
	.register_type::<(Color, Color, Color)>()
	.register_type::<PlayerAssets>()
	.register_type::<BoosterCharge>()
	.register_type::<WeaponCharge>()
	.register_type::<AbilityParams>()
	.register_type::<tune::PlayerCollider>()
	.register_type::<PlayerPhysicsParams>()
	.register_type::<PlayerParams>()
	.insert_resource(PlayerRespawnTimers::default())
	.add_systems(Startup, setup)
	.add_systems(
		First,
		update_player_spawn_data.run_if(resource_exists::<PlayerAssets>),
	)
	.add_systems(PreUpdate, Prev::<CtrlState>::update_component)
	.add_systems(
		Update,
		(
			ctrl::gravity
				.ambiguous_with(input::InputSystems) // Gravity only affects z, input only affects xy
				.before(terminal_velocity)
				.run_if(resource_exists::<PlayerParams>),
			camera::position_target.after(terminal_velocity),
			camera::follow_target.after(camera::position_target),
			ctrl::reset_jump_on_ground
				.before(input::InputSystems)
				.run_if(resource_exists::<PlayerParams>),
			ctrl::move_player
				.before(StepSimulation)
				.after(terminal_velocity)
				.after(input::InputSystems)
				.run_if(resource_exists::<PlayerParams>),
			idle,
			orbs_follow_arms.after(idle),
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
				.run_if(resource_exists::<PlayerParams>)
				.run_if(resource_exists::<PlayerSpawnData>),
		),
	)
	.add_event::<PlayerSpawnEvent>();
	app
}

pub fn setup(
	mut cmds: Commands,
	asset_server: Res<AssetServer>,
	mut images: ResMut<Assets<Image>>,
	settings: Res<Settings>,
) {
	cmds.insert_resource(PlayerBounds {
		aabb: Aabb::new(
			Point::new(-16384.0, -16384.0, -8192.0),
			Point::new(16384.0, 16384.0, 8192.0),
		),
	});

	cmds.insert_resource(Sfx {
		aoe: asset_server.load("sfx/SFX_-_magic_spell_03.ogg"),
		fire_a: asset_server.load("sfx/Kenney/Audio/laserSmall_004.ogg"),
		dash: asset_server.load("sfx/Kenney/Audio/forceField_000.ogg"),
		jump: asset_server.load("sfx/Kenney/Audio/forceField_002.ogg"),
		impacts: [
			asset_server.load("sfx/Kenney/Audio/impactMetal_000.ogg"),
			asset_server.load("sfx/Kenney/Audio/impactMetal_001.ogg"),
			asset_server.load("sfx/Kenney/Audio/impactMetal_002.ogg"),
			asset_server.load("sfx/Kenney/Audio/impactMetal_003.ogg"),
			asset_server.load("sfx/Kenney/Audio/impactMetal_004.ogg"),
		],
	});
	let Some(id) = PlayerId::new(1) else {
		unreachable!()
	};

	let ui_root = cmds
		.spawn((
			TransformBundle::from_transform(Transform {
				translation: Vec3::NEG_Z * 64.0,
				..default()
			}),
			VisibilityBundle::default(),
			RenderLayers::layer(player_ui_layer(id)),
			UiRoot,
		))
		.id();

	// TODO: Separate viewport for each player
	spawn_cameras(
		&mut cmds,
		id,
		&settings,
		&mut images,
		&asset_server,
		None,
		ui_root,
	);
}

pub fn update_player_spawn_data(
	mut cmds: Commands,
	mut std_mats: ResMut<Assets<StandardMaterial>>,
	mut meshes: ResMut<Assets<Mesh>>,
	asset_server: Res<AssetServer>,
	mut spawn_events: EventWriter<PlayerSpawnEvent>,
	player_assets: Res<PlayerAssets>,
	data: Option<ResMut<PlayerSpawnData>>,
) {
	if !player_assets.is_changed() {
		return;
	}

	let Some(id) = PlayerId::new(1) else {
		unreachable!()
	};

	let arm_mat_template = StandardMaterial {
		base_color: Color::NONE,
		reflectance: 0.0,
		double_sided: true,
		cull_mode: None,
		..default()
	};

	let PlayerAssets {
		ship_scene,
		antigrav_pulse_mesh,
		antigrav_pulse_mat,
		arm_mesh,
		arm_particle_radius,
		arm_mats,
		crosshair_mesh,
		crosshair_mat,
	} = player_assets.clone();

	if let Some(mut data) = data {
		data.ship_scene = asset_server.load(ship_scene);
		*meshes
			.get_mut(data.antigrav_pulse_mesh.clone_weak())
			.unwrap() = TorusMeshBuilder::from(antigrav_pulse_mesh).into();
		*std_mats
			.get_mut(data.antigrav_pulse_mat.clone_weak())
			.unwrap() = antigrav_pulse_mat;
		*meshes.get_mut(data.arm_mesh.clone_weak()).unwrap() =
			SphereMeshBuilder::from(arm_mesh).into();
		*meshes.get_mut(data.arm_particle_mesh.clone_weak()).unwrap() =
			RegularPolygon::new(arm_particle_radius, 3).into();
		*std_mats.get_mut(data.arm_mats[0].clone_weak()).unwrap() = StandardMaterial {
			emissive: arm_mats.0,
			..arm_mat_template.clone()
		};
		*std_mats.get_mut(data.arm_mats[1].clone_weak()).unwrap() = StandardMaterial {
			emissive: arm_mats.1,
			..arm_mat_template.clone()
		};
		*std_mats.get_mut(data.arm_mats[2].clone_weak()).unwrap() = StandardMaterial {
			emissive: arm_mats.2,
			..arm_mat_template.clone()
		};
		*meshes.get_mut(data.crosshair_mesh.clone_weak()).unwrap() =
			TorusMeshBuilder::from(crosshair_mesh).into();
		*std_mats.get_mut(data.crosshair_mat.clone_weak()).unwrap() = crosshair_mat;
		return;
	}
	// else insert spawn data
	let ship_scene = asset_server.load(ship_scene);
	let antigrav_pulse_mesh = Mesh::from(TorusMeshBuilder::from(antigrav_pulse_mesh));
	let antigrav_pulse_mesh = meshes.add(antigrav_pulse_mesh);
	let antigrav_pulse_mat = std_mats.add(antigrav_pulse_mat);
	let arm_mesh = Mesh::from(SphereMeshBuilder::from(arm_mesh));
	let arm_mesh = meshes.add(arm_mesh);
	let arm_particle_mesh = meshes.add(Mesh::from(RegularPolygon::new(arm_particle_radius, 3)));

	let arm_mats = [
		std_mats.add(StandardMaterial {
			emissive: arm_mats.0,
			..arm_mat_template.clone()
		}),
		std_mats.add(StandardMaterial {
			emissive: arm_mats.1,
			..arm_mat_template.clone()
		}),
		std_mats.add(StandardMaterial {
			emissive: arm_mats.2,
			..arm_mat_template
		}),
	];

	let crosshair_mesh = meshes.add(TorusMeshBuilder::from(crosshair_mesh));
	let crosshair_mat = std_mats.add(crosshair_mat);

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

	if player_assets.is_added() {
		spawn_events.send(PlayerSpawnEvent {
			id,
			died_at: PlanetVec2::default(),
		});
	}
}

#[derive(Clone, Reflect)]
pub struct ReflectTorus {
	pub major_radius: f32,
	pub minor_radius: f32,
	pub major_resolution: usize,
	pub minor_resolution: usize,
}

impl Default for ReflectTorus {
	fn default() -> Self {
		let TorusMeshBuilder {
			torus: Torus {
				minor_radius,
				major_radius,
			},
			minor_resolution,
			major_resolution,
		} = Torus::default().mesh();
		Self {
			minor_radius,
			major_radius,
			minor_resolution,
			major_resolution,
		}
	}
}

impl From<ReflectTorus> for TorusMeshBuilder {
	fn from(value: ReflectTorus) -> Self {
		Self {
			torus: Torus {
				major_radius: value.major_radius,
				minor_radius: value.minor_radius,
			},
			major_resolution: value.major_resolution,
			minor_resolution: value.minor_resolution,
		}
	}
}

#[derive(Clone, Reflect)]
pub struct ReflectIcosphere {
	pub radius: f32,
	pub subdivisions: usize,
}

impl Default for ReflectIcosphere {
	fn default() -> Self {
		Self {
			radius: Sphere::default().radius,
			subdivisions: 5,
		}
	}
}

impl From<ReflectIcosphere> for SphereMeshBuilder {
	fn from(value: ReflectIcosphere) -> Self {
		SphereMeshBuilder::new(
			value.radius,
			SphereKind::Ico {
				subdivisions: value.subdivisions,
			},
		)
	}
}

#[derive(Clone, Default, Resource, Reflect)]
#[reflect(Resource, from_reflect = false)]
pub struct PlayerAssets {
	pub ship_scene: AssetPath<'static>,
	pub antigrav_pulse_mesh: ReflectTorus,
	pub antigrav_pulse_mat: StandardMaterial,
	pub arm_mesh: ReflectIcosphere,
	pub arm_particle_radius: f32,
	pub arm_mats: (Color, Color, Color),
	pub crosshair_mesh: ReflectTorus,
	pub crosshair_mat: StandardMaterial,
}

#[derive(Resource, Debug, Default)]
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

impl PlayerSpawnData {
	pub fn clone_weak(&self) -> Self {
		Self {
			ship_scene: self.ship_scene.clone_weak(),
			antigrav_pulse_mesh: self.antigrav_pulse_mesh.clone_weak(),
			antigrav_pulse_mat: self.antigrav_pulse_mat.clone_weak(),
			arm_mesh: self.arm_mesh.clone_weak(),
			arm_particle_mesh: self.arm_particle_mesh.clone_weak(),
			arm_mats: [
				self.arm_mats[0].clone_weak(),
				self.arm_mats[1].clone_weak(),
				self.arm_mats[2].clone_weak(),
			],
			crosshair_mesh: self.crosshair_mesh.clone_weak(),
			crosshair_mat: self.crosshair_mat.clone_weak(),
		}
	}

	fn bundles(&self, id: PlayerId, transform: Transform) -> PlayerBundles {
		let Self {
			ship_scene,
			antigrav_pulse_mesh,
			antigrav_pulse_mat,
			arm_mesh,
			arm_particle_mesh,
			arm_mats: [mat_a, mat_b, mat_c],
			crosshair_mesh,
			crosshair_mat,
		} = self.clone_weak();

		let owner = BelongsToPlayer::with_id(id);
		let username = format!("Player{}", id.get());

		PlayerBundles {
			root: (
				Name::new(username.clone()),
				owner,
				TerminalVelocity(Velocity {
					linvel: Vect::splat(96.0),
					angvel: Vect::new(0.0, 0.0, PI / crate::DT), // Half a rotation per physics tick
				}),
				KinematicPositionBased,
				TransformBundle::from_transform(transform),
				Velocity::default(),
				Friction {
					coefficient: 0.0,
					combine_rule: Min,
				},
				VisibilityBundle::default(),
				NeedsTerrain,
			),
			vis: SceneBundle {
				scene: ship_scene,
				..default()
			},
			antigrav_pulse: PbrBundle {
				mesh: antigrav_pulse_mesh,
				material: antigrav_pulse_mat,
				..default()
			},
			arms: [
				(
					PbrBundle {
						mesh: arm_mesh.clone(),
						material: mat_a,
						transform: Transform::from_translation(Vec3::X * 2.0),
						..default()
					},
					PlayerArm::A,
				),
				(
					PbrBundle {
						mesh: arm_mesh.clone(),
						material: mat_b,
						transform: Transform::from_translation(
							Quat::from_rotation_z(FRAC_PI_3 * 2.0) * Vec3::X * 2.0,
						),
						..default()
					},
					PlayerArm::B,
				),
				(
					PbrBundle {
						mesh: arm_mesh,
						material: mat_c,
						transform: Transform::from_translation(
							Quat::from_rotation_z(FRAC_PI_3 * 4.0) * Vec3::X * 2.0,
						),
						..default()
					},
					PlayerArm::C,
				),
			],
			arm_particle_mesh,
			crosshair: PbrBundle {
				mesh: crosshair_mesh,
				material: crosshair_mat,
				transform: Transform::from_translation(Vec3::Y * 128.0),
				..default()
			},
		}
	}
}

struct PlayerBundles {
	root: (
		Name,
		BelongsToPlayer,
		TerminalVelocity,
		RigidBody,
		TransformBundle,
		Velocity,
		Friction,
		VisibilityBundle,
		NeedsTerrain,
	),
	vis: SceneBundle,
	antigrav_pulse: PbrBundle,
	arms: [(PbrBundle, PlayerArm); 3],
	arm_particle_mesh: Handle<Mesh>,
	crosshair: PbrBundle,
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
	mut pkv: ResMut<PkvStore>,
	chunks: ChunkFinder,
	frame: Res<Frame>,
	live_players: Query<&BelongsToPlayer, WithVariant<Root>>,
) {
	let mut to_retry = vec![];
	for event in events.drain() {
		if live_players.iter().any(|id| event.id == id.0) {
			error!("Player {} is already spawned", event.id.get());
			continue;
		}
		let spawn_point = frame.planet_coords_of(Vec2::ZERO);
		let Some(z) = chunks.height_at(spawn_point) else {
			to_retry.push(event);
			continue;
		};

		let id = event.id;
		let owner = BelongsToPlayer::with_id(id);
		let username = format!("Player{}", id.get());

		let PlayerBundles {
			root,
			vis,
			antigrav_pulse,
			arms,
			arm_particle_mesh,
			crosshair,
		} = spawn_data.bundles(
			id,
			Transform {
				translation: Vec3::new(0.0, 0.0, z + 2048.0),
				..default()
			},
		);

		let char_collider = params.phys.collider.into();
		let mut root = cmds.spawn(root).with_enum(Root);

		let prefs_key = format!("{username}.prefs");
		let prefs = match pkv.get(&prefs_key) {
			Ok(prefs) => prefs,
			Err(e) => {
				if !matches!(e, bevy_pkv::GetError::NotFound) {
					error!("{e}");
				}
				let prefs = PlayerPrefs::default();
				pkv.set(prefs_key, &prefs).unwrap_or_else(|e| error!("{e}"));
				prefs
			}
		};
		info!("Loaded: {prefs:#?}");

		populate_player_scene(
			&mut root,
			owner,
			vis,
			char_collider,
			antigrav_pulse,
			arms,
			arm_particle_mesh,
			crosshair,
			prefs,
		);
	}
	for event in to_retry.drain(..) {
		events.send(event);
	}
}

fn populate_player_scene(
	root: &mut EntityCommands,
	owner: BelongsToPlayer,
	vis: SceneBundle,
	char_collider: Collider,
	antigrav_pulse: PbrBundle,
	arm_meshes: [(PbrBundle, PlayerArm); 3],
	arm_particle_mesh: Handle<Mesh>,
	crosshair: PbrBundle,
	prefs: PlayerPrefs,
) {
	player_controller(root, owner, char_collider, prefs);
	player_vis(
		root,
		owner,
		vis,
		antigrav_pulse,
		arm_meshes,
		arm_particle_mesh,
		crosshair,
	);
}

fn player_controller(
	root: &mut EntityCommands,
	owner: BelongsToPlayer,
	char_collider: Collider,
	prefs: PlayerPrefs,
) {
	root.with_children(|builder| {
		let PlayerPrefs {
			camera: cam_prefs,
			input_map,
			ui_input_map,
		} = prefs;
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
				InputManagerBundle {
					input_map,
					..default()
				},
				InputManagerBundle {
					input_map: ui_input_map,
					..default()
				},
				CtrlState::default(),
				BoosterCharge::default(),
				WeaponCharge::default(),
				cam_prefs,
			))
			.with_enum(Controller);
	});
}

fn player_vis(
	root: &mut EntityCommands,
	owner: BelongsToPlayer,
	vis: SceneBundle,
	particle_mesh: PbrBundle,
	arm_meshes: [(PbrBundle, PlayerArm); 3],
	arm_particle_mesh: Handle<Mesh>,
	crosshair: PbrBundle,
) {
	let mut cmds = root.commands();
	let camera_pivot = camera::spawn_pivot(&mut cmds, owner, crosshair).id();
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
								intensity: 2_000_000.0,
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
	meshes: [(PbrBundle, PlayerArm); 3],
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
						#[cfg(target_arch = "wasm32")]
						lifetime: Lifetime(Duration::from_secs_f32(0.12)),
						#[cfg(not(target_arch = "wasm32"))]
						lifetime: Lifetime(Duration::from_secs_f32(0.18)),
					},
					Linear {
						velocity: Vec3::NEG_Z * 3.6,
					},
					TargetScale { scale: Vec3::ZERO },
				))
			}),
			#[cfg(target_arch = "wasm32")]
			interval: Duration::from_micros(3000),
			#[cfg(not(target_arch = "wasm32"))]
			interval: Duration::from_micros(1500),
			use_global_coords: true,
			..default()
		};
		let mut cmds = arms_pivot.commands();
		let mut orb = cmds
			.spawn((
				Name::new(format!(
					"Player{}.ShipCenter.Arms.{which:?}.Orb",
					owner.0.get()
				)),
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
	roots: Query<(&GlobalTransform, &BelongsToPlayer), WithVariant<Root>>,
	player_nodes: Query<(Entity, &BelongsToPlayer), (Without<NeverDespawn>, Without<Parent>)>,
	bounds: Res<PlayerBounds>,
	mut respawn_timers: ResMut<PlayerRespawnTimers>,
	frame: Res<Frame>,
) {
	let mut to_respawn = HashSet::new();
	for (xform, owner) in &q {
		if !bounds
			.aabb
			.contains_local_point(&xform.translation().into())
		{
			error!("Player {owner:?} is out of bounds. Respawning.");
			to_respawn.insert(owner);
		}
	}
	let mut started_timers = HashSet::new();
	for (id, owner) in &player_nodes {
		if to_respawn.contains(owner) {
			cmds.entity(id).despawn_recursive();
			if !started_timers.contains(owner) {
				started_timers.insert(*owner);
				roots.iter().find_map(|(global, id)| {
					(*id == *owner).then(|| {
						respawn_timers
							.start(
								**id,
								frame.planet_coords_of(global.translation().xy()),
								Duration::from_secs(3),
							)
							.ok()
					})
				});
			}
		}
	}
}

#[derive(Resource, Debug, Deref, DerefMut)]
pub struct PlayerBounds {
	pub aabb: Aabb,
}

pub fn idle(
	mut vis_q: Query<&mut Transform, WithVariant<Ship>>,
	mut arms_q: Query<(&mut Transform, &RotVel), WithVariant<Arms>>,
	mut arm_q: Query<&mut Transform, WithVariant<Arm>>,
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
		));
	}
}

#[derive(Event, Clone, Debug)]
pub struct PlayerSpawnEvent {
	pub id: PlayerId,
	pub died_at: PlanetVec2,
}

pub fn countdown_respawn(
	mut spawn_events: EventWriter<PlayerSpawnEvent>,
	mut timers: ResMut<PlayerRespawnTimers>,
	t: Res<Time>,
) {
	for (&id, (died_at, timer)) in timers.iter_mut() {
		timer.tick(t.delta());
		if timer.just_finished() {
			spawn_events.send(PlayerSpawnEvent {
				id,
				died_at: *died_at,
			});
		}
	}
}

pub fn kill_on_key(
	mut cmds: Commands,
	q: Query<(Entity, &BelongsToPlayer), (Without<NeverDespawn>, Without<Parent>)>,
	roots: Query<(&GlobalTransform, &BelongsToPlayer), WithVariant<Root>>,
	input: Res<ButtonInput<KeyCode>>,
	mut respawn_timers: ResMut<PlayerRespawnTimers>,
	frame: Res<Frame>,
) {
	if input.just_pressed(KeyCode::KeyK) {
		for (id, owner) in &q {
			cmds.entity(id).despawn_recursive();
			roots.iter().find_map(|(global, id)| {
				(*id == *owner).then(|| {
					respawn_timers
						.start(
							**id,
							frame.planet_coords_of(global.translation().xy()),
							Duration::from_secs(2),
						)
						.ok()
				})
			});
		}
	}
}

pub fn play_death_sound(
	sound: Res<MissSfx>,
	audio: Res<Audio>,
	respawn_timers: Res<PlayerRespawnTimers>,
) {
	for (_, timer) in respawn_timers.values() {
		if timer.elapsed() == Duration::ZERO {
			audio.play(sound.0.clone()).with_volume(0.4);
		}
	}
}

#[derive(Resource, Debug, Default, Deref, DerefMut)]
pub struct PlayerRespawnTimers(HashMap<PlayerId, (PlanetVec2, Timer)>);

impl PlayerRespawnTimers {
	pub fn start(
		&mut self,
		player: PlayerId,
		death_point: PlanetVec2,
		duration: Duration,
	) -> Result<(), Duration> {
		let (_, timer) = self
			.0
			.entry(player)
			.or_insert_with(|| (death_point, Timer::new(duration, TimerMode::Once)));
		if timer.finished() {
			timer.set_duration(duration);
			timer.reset();
			Ok(())
		} else {
			Err(timer.remaining())
		}
	}
}
