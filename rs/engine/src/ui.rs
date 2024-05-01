use crate::{
	anim::{AnimationController, ComponentDelta, StartAnimation},
	ui::widgets::{Button3d, Font3d, PanelBuilder, TextBuilder, WidgetShape},
	util::{Diff, Prev},
};
use bevy::render::camera::Viewport;
use bevy::{
	asset::{io::Reader, AssetLoader, BoxedFuture, LoadContext},
	core_pipeline::{experimental::taa::TemporalAntiAliasBundle, fxaa::Fxaa},
	diagnostic::{DiagnosticsStore, FrameTimeDiagnosticsPlugin},
	ecs::{query::QuerySingleError, schedule::SystemConfigs},
	input::common_conditions::input_toggle_active,
	prelude::*,
	render::view::{Layer, RenderLayers},
	ui::FocusPolicy,
	utils::{CowArc, HashMap},
};
use futures_lite::AsyncReadExt;
use leafwing_input_manager::{prelude::*, Actionlike};
use meshtext::{MeshGenerator, QualitySettings};
use serde::{Deserialize, Serialize};
use std::f64::consts::TAU;

pub mod a11y;
#[cfg(feature = "debugging")]
pub mod dbg;
pub mod layout;
pub mod widgets;

pub const GLOBAL_UI_LAYER: Layer = (RenderLayers::TOTAL_LAYERS - 1) as Layer;
pub const GLOBAL_UI_RENDER_LAYERS: RenderLayers = RenderLayers::layer(GLOBAL_UI_LAYER);
pub const ALL_UI_RENDER_LAYERS: RenderLayers = RenderLayers::all().without(0);

pub struct UiPlugin;

impl Plugin for UiPlugin {
	fn build(&self, app: &mut App) {
		#[cfg(feature = "debugging")]
		app.add_plugins(dbg::DebugUiPlugin)
			.add_systems(PreUpdate, spawn_test_menu)
			.add_systems(Update, toggle_test_menu);

		app.world.spawn((
			GlobalUi,
			UiRoot,
			TransformBundle::default(),
			VisibilityBundle::default(),
		));

		app.init_resource::<UiHovered>()
			.init_resource::<TextMeshCache>()
			.init_asset::<Font3d>()
			.register_asset_loader(Font3dLoader)
			.add_systems(Startup, setup)
			.add_systems(Update, (reset_hovered, show_fps))
			.add_systems(PostUpdate, layout::apply_constraints)
			.add_systems(
				Last,
				(Prev::<Button3d>::update_component, hide_orphaned_ui_nodes),
			);
	}

	fn finish(&self, app: &mut App) {
		let srv = app.world.resource::<AssetServer>();
		let mono = srv.load("ui/fonts/KodeMono/static/KodeMono-Bold.ttf");
		let mono_3d = srv.load("ui/fonts/Noto_Sans_Mono/static/NotoSansMono-Bold.ttf");
		app.insert_resource(UiFonts { mono, mono_3d });
		app.world.resource_mut::<Assets<StandardMaterial>>().insert(
			widgets::DEFAULT_TEXT_MAT,
			StandardMaterial {
				base_color: Color::WHITE,
				unlit: true,
				..default()
			},
		);
	}
}

pub fn setup(mut cmds: Commands) {
	spawn_ui_camera(cmds.reborrow(), GlobalUi, GLOBAL_UI_LAYER, None, 0.0);

	// Lights add together to white in front, but tint the sides of UI elements
	cmds.spawn((
		DirectionalLightBundle {
			directional_light: DirectionalLight {
				color: Color::rgb(0.5, 0.75, 0.125),
				illuminance: 5000.0,
				..default()
			},
			transform: Transform::from_rotation(Quat::from_rotation_arc(
				Vec3::NEG_Z,
				Vec3::new(-0.05, 1.0, -0.05).normalize(),
			)),
			..default()
		},
		ALL_UI_RENDER_LAYERS,
	));
	cmds.spawn((
		DirectionalLightBundle {
			directional_light: DirectionalLight {
				color: Color::rgb(0.5, 0.25, 0.875),
				illuminance: 5000.0,
				..default()
			},
			transform: Transform::from_rotation(Quat::from_rotation_arc(
				Vec3::NEG_Z,
				Vec3::new(0.05, 1.0, -0.05).normalize(),
			)),
			..default()
		},
		ALL_UI_RENDER_LAYERS,
	));

	cmds.spawn((
		DirectionalLightBundle {
			directional_light: DirectionalLight {
				illuminance: 400.0,
				color: Color::rgb(1.0, 0.125, 1.0),
				..default()
			},
			transform: Transform::from_rotation(Quat::from_rotation_arc(Vec3::NEG_Z, Vec3::Z)),
			..default()
		},
		ALL_UI_RENDER_LAYERS,
	));
}

pub fn spawn_ui_camera<ID: Bundle + Clone>(
	mut cmds: Commands,
	identifier: ID,
	layer: Layer,
	viewport: Option<Viewport>,
	animation_time_offset: f64,
) {
	let cam_pos = Vec3::new(0.0, -16.0, 0.0);
	let layers = RenderLayers::layer(layer);
	cmds.spawn((
		identifier.clone(),
		CamAnchor,
		TransformBundle::default(),
		VisibilityBundle::default(),
		layers,
	))
	.with_children(|cmds| {
		let mut global_ui_cam = cmds.spawn((
			identifier,
			UiCam::default(),
			Camera3dBundle {
				camera: Camera {
					hdr: true,
					order: layer as _,
					clear_color: ClearColorConfig::None,
					viewport,
					..default()
				},
				projection: PerspectiveProjection {
					fov: std::f32::consts::FRAC_PI_2,
					..default()
				}
				.into(),
				transform: Transform {
					translation: cam_pos,
					rotation: Quat::from_rotation_arc(
						// default forward
						Vec3::NEG_Z,
						// desired forward
						-cam_pos.normalize(),
					),
					..default()
				},
				..default()
			},
			TemporalAntiAliasBundle::default(),
			Fxaa::default(),
			layers,
		));

		global_ui_cam.start_animation::<Transform>(cam_idle_animation(animation_time_offset));
	});
}

/// Add this component to each entity that should be the root of a player's UI tree.
#[derive(Component, Reflect, Copy, Clone, Debug)]
#[reflect(Component)]
pub struct UiRoot;

/// Marker for entities that belong to the global UI (not tied to a player).
#[derive(Component, Reflect, Copy, Clone, Debug)]
#[reflect(Component)]
pub struct GlobalUi;

pub type IsGlobalUiRoot = (With<UiRoot>, With<GlobalUi>);

/// Component for any UI camera entity, player or global.
#[derive(Component, Default, Copy, Clone, Debug)]
pub struct UiCam {
	pub focus: Option<Entity>,
}

/// Add this component to the parent of each UI camera
#[derive(Component, Reflect, Copy, Clone, Debug)]
pub struct CamAnchor;

#[derive(Resource, Clone, Debug)]
pub struct UiFonts {
	pub mono: Handle<Font>,
	pub mono_3d: Handle<Font3d>,
}

#[derive(Resource, Default, Clone, Debug, Deref, DerefMut)]
pub struct TextMeshCache(
	pub  HashMap<
		(CowArc<'static, str>, [u32; 16], Handle<Font3d>),
		Option<(Handle<Mesh>, WidgetShape)>,
	>,
);

/// Keeps track of whether a UI element is hovered over so that clicking
/// does not grab the mouse if so.
#[derive(Resource, Debug, Default, Copy, Clone, Deref, DerefMut, Reflect)]
pub struct UiHovered(bool);

#[derive(
	Actionlike,
	Copy,
	Clone,
	Debug,
	PartialEq,
	Eq,
	PartialOrd,
	Ord,
	Hash,
	Reflect,
	Serialize,
	Deserialize,
)]
pub enum UiAction {
	Ok,
	Back,
	MoveCursor,
	FocusNext,
	FocusPrev,
	NextTab,
	PrevTab,
}

impl UiAction {
	pub fn default_mappings() -> InputMap<Self> {
		use KeyCode::*;
		use UiAction::*;
		InputMap::new([
			// KB & Mouse
			(Ok, Space.into()),
			(Ok, Backspace.into()),
			(Back, Backspace.into()),
			(MoveCursor, VirtualDPad::wasd().into()),
			(MoveCursor, VirtualDPad::arrow_keys().into()),
			(FocusNext, Tab.into()),
			(
				FocusPrev,
				UserInput::Chord(vec![ShiftLeft.into(), Tab.into()]),
			),
			(
				FocusPrev,
				UserInput::Chord(vec![ShiftRight.into(), Tab.into()]),
			),
			// Controller
			(Ok, GamepadButtonType::South.into()),
			(Back, GamepadButtonType::East.into()),
			(MoveCursor, VirtualDPad::dpad().into()),
			(MoveCursor, DualAxis::left_stick().into()),
			(MoveCursor, DualAxis::right_stick().into()),
			(FocusNext, GamepadButtonType::RightTrigger.into()),
			(FocusPrev, GamepadButtonType::LeftTrigger.into()),
			(NextTab, GamepadButtonType::RightTrigger2.into()),
			(PrevTab, GamepadButtonType::LeftTrigger2.into()),
		])
	}
}

pub struct Font3dLoader;
#[derive(Copy, Clone, Debug, Reflect, Serialize, Deserialize)]
#[reflect(Serialize, Deserialize)]
#[serde(default)]
pub struct Font3dLoaderSettings {
	pub quad_interpolation_steps: u32,
	pub cubic_interpolation_steps: u32,
}

impl Default for Font3dLoaderSettings {
	fn default() -> Self {
		Self {
			quad_interpolation_steps: 5,
			cubic_interpolation_steps: 3,
		}
	}
}

impl AssetLoader for Font3dLoader {
	type Asset = Font3d;
	type Settings = Font3dLoaderSettings;
	type Error = std::io::Error;

	fn load<'a>(
		&'a self,
		reader: &'a mut Reader,
		settings: &'a Self::Settings,
		_: &'a mut LoadContext,
	) -> BoxedFuture<'a, Result<Self::Asset, Self::Error>> {
		let Font3dLoaderSettings {
			quad_interpolation_steps,
			cubic_interpolation_steps,
		} = *settings;
		Box::pin(async move {
			let mut buf = vec![];
			reader.read_to_end(&mut buf).await?;
			Ok(Font3d(MeshGenerator::new_without_cache(
				buf,
				QualitySettings {
					quad_interpolation_steps,
					cubic_interpolation_steps,
				},
			)))
		})
	}

	fn extensions(&self) -> &[&str] {
		&["ttf"]
	}
}

pub fn hide_orphaned_ui_nodes(
	mut q: Query<(Option<&Parent>, &mut Visibility), With<WidgetShape>>,
	// Popups are spawned on `CamAnchor`
	roots: Query<(), Or<(With<UiRoot>, With<CamAnchor>)>>,
) {
	for (parent, mut vis) in &mut q {
		match (parent.map(Parent::get), *vis) {
			(Some(parent), Visibility::Hidden) if roots.contains(parent) => {
				*vis = Visibility::Inherited
			}
			(None, Visibility::Inherited | Visibility::Visible) => *vis = Visibility::Hidden,
			_ => {}
		}
	}
}

pub fn cam_idle_animation(
	time_offset: f64,
) -> impl FnMut(Entity, Ref<Transform>, Res<Time>, AnimationController) -> ComponentDelta<Transform>
       + Send
       + Sync
       + 'static {
	let (mut loop_t_x, mut loop_t_z) = (time_offset, time_offset);
	move |id, xform, t, _| {
		let dt = t.delta_seconds_f64();
		loop_t_x = (loop_t_x + (dt * std::f64::consts::FRAC_1_SQRT_2)) % TAU;
		loop_t_z = (loop_t_z + dt) % TAU;
		let x = loop_t_x.sin() * 0.32;
		let z = loop_t_z.cos() * 0.32;
		let new_pos = Vec3::new(x as f32, xform.translation.y, z as f32);
		let new_rot = Quat::from_rotation_arc(Vec3::NEG_Z, -new_pos.normalize());
		ComponentDelta::<Transform>::new(id, f32::NAN, move |mut xform, coef| {
			*xform = *xform
				+ Transform {
					translation: new_pos,
					rotation: new_rot,
					scale: xform.scale,
				}
				.delta_from(&xform) * coef;
		})
	}
}

/// Basic FPS viewer for release builds.
#[derive(Component, Default, Copy, Clone, Debug, Reflect)]
pub struct FpsViewer;

pub fn show_fps(
	mut cmds: Commands,
	keys: Res<ButtonInput<KeyCode>>,
	diags: Res<DiagnosticsStore>,
	mut q: Query<(Entity, &mut Text), With<FpsViewer>>,
	ui_fonts: Res<UiFonts>,
) {
	if keys.just_pressed(KeyCode::F10) {
		match q.get_single() {
			Err(QuerySingleError::NoEntities(_)) => {
				let val = diags
					.get_measurement(&FrameTimeDiagnosticsPlugin::FPS)
					.map_or(f64::NAN, |meas| meas.value);
				cmds.spawn((
					TextBundle {
						text: Text::from_section(
							format!("{val:.2}"),
							TextStyle {
								font: ui_fonts.mono.clone(),
								font_size: 24.0,
								color: Color::YELLOW,
							},
						),
						style: Style {
							position_type: PositionType::Absolute,
							right: Val::ZERO,
							..default()
						},
						focus_policy: FocusPolicy::Pass,
						..default()
					},
					FpsViewer,
				));
			}
			Ok((id, _)) => {
				cmds.entity(id).despawn();
			}
			Err(e) => error!("{e}"),
		}
		return;
	}

	for (_, mut text) in &mut q {
		let val = diags
			.get_measurement(&FrameTimeDiagnosticsPlugin::FPS)
			.map_or(f64::NAN, |meas| meas.value);
		text.sections.first_mut().unwrap().value = format!("{val:.2}");
	}
}

// Debug UI helpers, handling `#[cfg(...)]` internally for simpler setup.

#[derive(SystemSet, Copy, Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ShowDebugWindows;

pub trait AddDebugUi {
	/// Like `App::add_systems`, but disables the systems when the `debugging` feature
	/// is not enabled.
	fn add_debug_systems<M>(&mut self, systems: impl IntoSystemConfigs<M>) -> &mut Self;
}

impl AddDebugUi for App {
	#[inline(always)]
	fn add_debug_systems<M>(&mut self, _systems: impl IntoSystemConfigs<M>) -> &mut Self {
		#[cfg(feature = "debugging")]
		self.add_systems(Update, _systems.in_set(ShowDebugWindows));
		self
	}
}

pub trait ToggleDbgUi<M> {
	/// Debug UI window will be hidden by default, and can be shown by pressing the given
	/// key while the debug interface is visible.
	fn show_with(self, key: KeyCode) -> SystemConfigs;
	/// Debug UI window will be visible by default, and can be hidden by pressing the given
	/// key while the debug interface is visible.
	fn hide_with(self, key: KeyCode) -> SystemConfigs;
}

impl<S: IntoSystemConfigs<M>, M> ToggleDbgUi<M> for S {
	fn show_with(self, key: KeyCode) -> SystemConfigs {
		self.run_if(dbg_window_toggled(false, key))
	}

	fn hide_with(self, key: KeyCode) -> SystemConfigs {
		self.run_if(dbg_window_toggled(true, key))
	}
}

pub fn dbg_window_toggled(default: bool, code: KeyCode) -> impl Condition<()> {
	input_toggle_active(false, KeyCode::Backquote).and_then(input_toggle_active(default, code))
}

pub fn reset_hovered(mut ui_hovered: ResMut<UiHovered>) {
	if **ui_hovered {
		**ui_hovered = false;
	}
}

#[cfg(feature = "debugging")]
fn spawn_test_menu(
	mut cmds: Commands,
	mut meshes: ResMut<Assets<Mesh>>,
	mut mats: ResMut<Assets<StandardMaterial>>,
	mut cache: ResMut<TextMeshCache>,
	mut fonts: ResMut<Assets<Font3d>>,
	ui_fonts: Res<UiFonts>,
	mut events: EventReader<AssetEvent<Font3d>>,
	mut spawned: Local<bool>,
) {
	// TODO: Use bevy_asset_loader
	if *spawned {
		return;
	};
	let mut loaded = false;
	for e in events.read() {
		if let AssetEvent::Added { id } = e {
			if *id == ui_fonts.mono_3d.id() {
				loaded = true;
			}
		}
	}
	if !loaded {
		return;
	};

	cmds.spawn((
		PanelBuilder {
			size: Vec3::new(16.0, 1.0, 9.0),
			visibility: Visibility::Hidden,
			material: mats.add(StandardMaterial {
				base_color: Color::rgba(0.1, 0.1, 0.1, 0.9),
				reflectance: 0.0,
				alpha_mode: AlphaMode::Blend,
				..default()
			}),
			..default()
		}
		.build(&mut meshes)
		.0,
		TestMenu,
	))
	.with_children(|cmds| {
		cmds.spawn(
			TextBuilder {
				text: "Testing...".into(),
				font: ui_fonts.mono_3d.clone(),
				transform: Transform::from_translation(Vec3::NEG_Y),
				..default()
			}
			.build(meshes.reborrow(), cache.reborrow(), fonts.reborrow())
			.unwrap(),
		);
	});
	*spawned = true;
}

#[cfg(feature = "debugging")]
#[derive(Component)]
struct TestMenu;

#[cfg(feature = "debugging")]
fn toggle_test_menu(
	mut cmds: Commands,
	root: Query<Entity, IsGlobalUiRoot>,
	q: Query<(Entity, Has<Parent>), With<TestMenu>>,
	input: Res<ButtonInput<KeyCode>>,
) {
	let Ok(root) = root.get_single() else { return };
	let Ok((menu, has_parent)) = q.get_single() else {
		return;
	};
	if input.just_pressed(KeyCode::KeyM) {
		let mut cmds = cmds.entity(menu);
		if has_parent {
			cmds.remove_parent();
		} else {
			cmds.set_parent(root);
		}
	}
}
