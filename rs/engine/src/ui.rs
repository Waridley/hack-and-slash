use crate::{
	anim::{AnimationController, AnimationPlugin, ComponentDelta, StartAnimation},
	entity_tree,
	input::InputState,
	mats::{
		fade::DitherFade,
		fog::{DistanceDither, Matter},
		ExtMat,
	},
	ui::{
		a11y::AKNode,
		focus::{highlight_focus, AdjacentWidgets, FocusTarget, Wedge2d},
		layout::LineUpChildren,
		widgets::{
			draw_widget_shape_gizmos, Button3d, Button3dBundle, CuboidFaces, CuboidPanel,
			CuboidPanelBundle, Font3d, Node3dBundle, RectBorderDesc, RectCorners, Text3d,
			Text3dBundle, WidgetBundle, WidgetShape,
		},
	},
	util::{CompassDirection, Diff, LerpSlerp, Prev, StateStack},
};
use bevy::{
	a11y::Focus,
	asset::{io::Reader, AssetLoader, BoxedFuture, LoadContext},
	core_pipeline::{experimental::taa::TemporalAntiAliasBundle, fxaa::Fxaa},
	diagnostic::{DiagnosticsStore, FrameTimeDiagnosticsPlugin},
	ecs::{
		query::{QueryEntityError, QuerySingleError},
		schedule::SystemConfigs,
		system::EntityCommands,
	},
	input::common_conditions::input_toggle_active,
	pbr::ExtendedMaterial,
	prelude::*,
	render::{
		camera::Viewport,
		view::{Layer, RenderLayers, VisibilitySystems::CheckVisibility},
	},
	ui::FocusPolicy,
	utils::{CowArc, HashMap},
};
use futures_lite::AsyncReadExt;
use leafwing_input_manager::{prelude::*, Actionlike};
use meshtext::{MeshGenerator, QualitySettings};
use rapier3d::{geometry::SharedShape, math::Point};
use serde::{Deserialize, Serialize};
use std::{
	collections::VecDeque,
	f64::consts::TAU,
	ops::{Add, Mul},
};

pub mod a11y;
#[cfg(feature = "debugging")]
pub mod dbg;
pub mod focus;
pub mod layout;
pub mod widgets;

pub const GLOBAL_UI_LAYER: Layer = (RenderLayers::TOTAL_LAYERS - 1) as Layer;
pub const GLOBAL_UI_RENDER_LAYERS: RenderLayers = RenderLayers::layer(GLOBAL_UI_LAYER);
pub const ALL_UI_RENDER_LAYERS: RenderLayers = RenderLayers::all().without(0);

pub type UiMat = ExtMat<DitherFade>;

pub struct UiPlugin;

impl Plugin for UiPlugin {
	fn build(&self, app: &mut App) {
		#[cfg(feature = "debugging")]
		app.add_plugins(dbg::DebugUiPlugin)
			.add_systems(PreUpdate, spawn_test_menu)
			.add_systems(Update, toggle_test_menu)
			.add_systems(
				PostUpdate,
				draw_widget_shape_gizmos::<GLOBAL_UI_LAYER>
					.run_if(input_toggle_active(false, KeyCode::KeyG))
					.after(CheckVisibility),
			)
			.insert_gizmo_group(
				widgets::WidgetGizmos::<GLOBAL_UI_LAYER>,
				GizmoConfig {
					render_layers: GLOBAL_UI_RENDER_LAYERS,
					..default()
				},
			);

		app.add_plugins((
			InputManagerPlugin::<UiAction>::default(),
			AnimationPlugin::<Fade>::default(),
		))
		.register_type::<MenuStack>()
		.register_type::<UiCam>()
		.register_type::<Fade>()
		.init_resource::<ActionState<UiAction>>()
		.insert_resource(UiAction::default_mappings())
		.init_resource::<UiHovered>()
		.init_resource::<TextMeshCache>()
		.init_asset::<Font3d>()
		.register_asset_loader(Font3dLoader)
		.add_systems(Startup, setup)
		.add_systems(Update, (reset_hovered, show_fps, focus::resolve_focus))
		.add_systems(
			PostUpdate,
			(
				layout::apply_constraints,
				highlight_focus::<GLOBAL_UI_LAYER>,
				widgets::InteractHandlers::system,
			),
		)
		.add_systems(
			Last,
			(
				Prev::<Button3d>::update_component,
				hide_orphaned_popups,
				propagate_fade::<UiMat>.before(Fade::hide_faded_out),
				Fade::hide_faded_out,
				anchor_follow_menu,
				CuboidPanel::<UiMat>::sync,
				Text3d::sync,
			),
		)
		.insert_gizmo_group(
			focus::FocusGizmos::<GLOBAL_UI_LAYER>,
			GizmoConfig {
				line_width: 6.0,
				render_layers: GLOBAL_UI_RENDER_LAYERS,
				..default()
			},
		);
	}

	fn finish(&self, app: &mut App) {
		let srv = app.world.resource::<AssetServer>();
		let mono = srv.load("ui/fonts/KodeMono/static/KodeMono-Bold.ttf");
		let mono_3d = srv.load("ui/fonts/Noto_Sans_Mono/static/NotoSansMono-Bold.ttf");
		app.insert_resource(UiFonts { mono, mono_3d });
		app.world.resource_mut::<Assets<_>>().insert(
			Handle::weak_from_u128(widgets::UNLIT_MATERIAL_ID),
			new_unlit_material(),
		);
	}
}

impl DistanceDither {
	pub fn ui() -> Self {
		Self {
			near_start: 5.0,
			..default()
		}
	}
}

pub fn setup(mut cmds: Commands) {
	spawn_ui_camera(cmds.reborrow(), GlobalUi, GLOBAL_UI_LAYER, None, 0.0);
}

pub fn spawn_ui_camera<ID: Bundle + Clone>(
	mut cmds: Commands,
	identifier: ID,
	layer: Layer,
	viewport: Option<Viewport>,
	animation_time_offset: f64,
) {
	let cam_pos = Vec3::new(0.0, -20.0, 0.0);
	let layers = RenderLayers::layer(layer);
	cmds.spawn((
		identifier.clone(),
		CamAnchor,
		TransformBundle::default(),
		VisibilityBundle::default(),
		MenuStack::default(),
		FocusStack::default(),
		layers,
	))
	.with_children(|cmds| {
		cmds.spawn((
			identifier.clone(),
			PopupsRoot,
			TransformBundle::from_transform(Transform {
				translation: Vec3::NEG_Y * 10.0,
				..default()
			}),
			VisibilityBundle::default(),
			layers,
		));

		let mut cam = cmds.spawn((
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
					fov: std::f32::consts::FRAC_PI_3,
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

		cam.start_animation::<Transform>(cam_idle_animation(animation_time_offset));

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
			layers,
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
			layers,
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
			layers,
		));
	});
}

/// Marker for entities that belong to the global UI (not tied to a player).
#[derive(Component, Reflect, Copy, Clone, Debug)]
#[reflect(Component)]
pub struct GlobalUi;

/// Component for any UI camera entity, player or global.
#[derive(Component, Default, Copy, Clone, Debug, Reflect)]
#[reflect(Component)]
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
	Opt1,
	Opt2,
	Back,
	MoveCursor,
	FocusNext,
	FocusPrev,
	NextTab, // TODO: Should these be an Axis or even DualAxis instead?
	PrevTab,
	Zoom,
	Pan,
}

impl UiAction {
	pub fn default_mappings() -> InputMap<Self> {
		use KeyCode::*;
		use UiAction::*;
		InputMap::new([
			// KB & Mouse
			(Ok, Space.into()),
			(Ok, Backspace.into()),
			(Ok, Enter.into()),
			(Back, Backspace.into()),
			(MoveCursor, VirtualDPad::wasd().into()),
			(MoveCursor, VirtualDPad::arrow_keys().into()),
			(FocusNext, Tab.into()),
			(
				FocusPrev,
				UserInput::Chord(vec![Modifier::Shift.into(), Tab.into()]),
			),
			(
				Zoom,
				UserInput::Chord(vec![
					Modifier::Control.into(),
					SingleAxis::mouse_wheel_y().into(),
				]),
			),
			// Controller
			(Ok, GamepadButtonType::South.into()),
			(Back, GamepadButtonType::East.into()),
			(MoveCursor, VirtualDPad::dpad().into()),
			(MoveCursor, DualAxis::left_stick().into()),
			(NextTab, GamepadButtonType::RightTrigger.into()),
			(PrevTab, GamepadButtonType::LeftTrigger.into()),
			(
				Zoom,
				UserInput::Chord(vec![
					GamepadButtonType::LeftTrigger2.into(),
					SingleAxis::symmetric(GamepadAxisType::RightStickY, 0.1).into(),
				]),
			),
			(Pan, DualAxis::right_stick().into()),
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

/// Marker for the parents of all popup menus.
#[derive(Component, Debug, Reflect)]
#[reflect(Component)]
pub struct PopupsRoot;

/// Marker for Popup entities. Required for automatic visibility management.
/// Popups can display on top of any UI element anywhere in the world.
/// Thus they are added as children of the [PopupsRoot] which is always in
/// front of the camera.
#[derive(Component, Debug, Reflect)]
#[reflect(Component)]
pub struct Popup;

pub fn hide_orphaned_popups(
	mut q: Query<(Option<&Parent>, &mut Visibility), With<Popup>>,
	// Popups are spawned on `CamAnchor`
	roots: Query<(), With<PopupsRoot>>,
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

#[derive(Component, Default, Debug, Reflect, Deref, DerefMut)]
#[reflect(Component)]
pub struct MenuStack(pub Vec<Entity>);

#[derive(Component, Default, Debug, Reflect, Deref, DerefMut)]
#[reflect(Component)]
pub struct FocusStack(pub Vec<Entity>);

pub fn anchor_follow_menu(
	mut q: Query<(&mut Transform, &MenuStack)>,
	nodes: Query<&GlobalTransform, Without<MenuStack>>,
	t: Res<Time>,
) {
	for (mut xform, stack) in &mut q {
		if let Some(node) = stack.last() {
			let target = match nodes.get(*node) {
				Ok(node) => node,
				Err(e) => {
					error!("{e}");
					continue;
				}
			};

			let new = xform.lerp_slerp(target.compute_transform(), t.delta_seconds() * 3.0);
			if *xform != new {
				*xform = new;
			}
		}
	}
}

use crate::{
	anim::{AnimationHandle, Delta, DynAnimation},
	ui::widgets::new_unlit_material,
};
#[cfg(feature = "debugging")]
use bevy_inspector_egui::{
	inspector_options::std_options::NumberDisplay::Slider,
	prelude::{InspectorOptions, ReflectInspectorOptions},
};
use web_time::Duration;

/// Component that starts a new branch of a tree of entities that can be
/// faded in an out together.
///
/// Note: Materials should not be shared between different branches of
/// fade-able trees, otherwise fading one branch can affect entities from
/// another. In particular, this means the default material should
/// generally not be used for any fade-able entities.
#[derive(Component, Debug, Copy, Clone, PartialEq, PartialOrd, Reflect, Deref, DerefMut)]
#[cfg_attr(
	feature = "debugging",
	derive(InspectorOptions),
	reflect(InspectorOptions)
)]
#[reflect(Component)]
pub struct Fade(
	#[cfg_attr(feature = "debugging", inspector(
		min = 0.0,
		max = 1.0,
		speed = 0.00389, // 1.0 / 257.0
		display = Slider,
	))]
	pub f32,
);

impl Fade {
	pub const ZERO: Self = Self(0.0);
	pub const ONE: Self = Self(1.0);

	pub fn hide_faded_out(mut q: Query<(&Self, &mut Visibility), Changed<Self>>) {
		for (fade, mut vis) in &mut q {
			let new = if fade.0 <= 0.0 {
				Visibility::Hidden
			} else {
				Visibility::Inherited
			};
			if *vis != new {
				*vis = new;
			}
		}
	}
}

impl Default for Fade {
	fn default() -> Self {
		Self(1.0)
	}
}

impl Diff for Fade {
	type Delta = Self;

	fn delta_from(&self, rhs: &Self) -> Self::Delta {
		Self(self.0 - rhs.0)
	}
}

impl Mul<f32> for Fade {
	type Output = Self;

	fn mul(self, rhs: f32) -> Self::Output {
		Self(self.0 * rhs)
	}
}

impl Add for Fade {
	type Output = Self;

	fn add(self, rhs: Self) -> Self::Output {
		Self(self.0 + rhs.0)
	}
}

pub fn propagate_fade<M: Asset + AsMut<DitherFade>>(
	q: Query<&Handle<M>>,
	roots: Query<(Entity, Ref<Fade>)>,
	children_q: Query<&Children>,
	mut mats: ResMut<Assets<M>>,
) {
	for (root, fade) in &roots {
		if !fade.is_changed() {
			continue;
		}

		let mut set_fade = |handle: &Handle<M>| {
			let Some(mut mat) = mats.get_mut(handle.clone()) else {
				warn!("missing {handle:?}");
				return;
			};
			mat.as_mut().fade = **fade;
		};

		if let Ok(mat) = q.get(root) {
			set_fade(mat)
		}

		let Ok(children) = children_q.get(root) else {
			continue;
		};

		// Manually build queue instead of using `iter_descendants` to filter
		// out children with `Fade` components which override their ancestor's.
		let mut queue = children
			.into_iter()
			.copied()
			.filter(|&child| !roots.contains(child))
			.collect::<VecDeque<_>>();

		loop {
			let Some(child) = queue.pop_front() else {
				break;
			};
			if let Ok(mat) = q.get(child) {
				set_fade(mat);
			}
			if let Ok(children) = children_q.get(child) {
				queue.extend(
					children
						.into_iter()
						.copied()
						.filter(|&child| !roots.contains(child)),
				);
			}
		}
	}
}

pub trait FadeCommands {
	fn fade_in(&mut self, duration: Duration) -> AnimationHandle<DynAnimation<Fade>>;
	fn fade_out(&mut self, duration: Duration) -> AnimationHandle<DynAnimation<Fade>>;

	fn fade_in_secs(&mut self, secs: f32) -> AnimationHandle<DynAnimation<Fade>> {
		self.fade_in(Duration::from_secs_f32(secs))
	}
	fn fade_out_secs(&mut self, secs: f32) -> AnimationHandle<DynAnimation<Fade>> {
		self.fade_out(Duration::from_secs_f32(secs))
	}
}

impl FadeCommands for EntityCommands<'_> {
	fn fade_in(&mut self, duration: Duration) -> AnimationHandle<DynAnimation<Fade>> {
		let mut elapsed = Duration::ZERO;
		let duration = duration.as_secs_f32();
		self.start_animation(move |id, fade, t, mut ctrl| {
			elapsed += t.delta();
			let t = elapsed.as_secs_f32() / duration;
			let t = if t >= 1.0 {
				ctrl.end();
				1.0
			} else {
				t
			};
			ComponentDelta::diffable(id, t, Fade(t))
		})
	}

	fn fade_out(&mut self, duration: Duration) -> AnimationHandle<DynAnimation<Fade>> {
		let mut elapsed = Duration::ZERO;
		let duration = duration.as_secs_f32();
		self.start_animation(move |id, fade, t, mut ctrl| {
			elapsed += t.delta();
			let t = 1.0 - (elapsed.as_secs_f32() / duration);
			let t = if t <= 0.0 {
				ctrl.end();
				0.0
			} else {
				t
			};
			ComponentDelta::diffable(id, t, Fade(t))
		})
	}
}

#[cfg(feature = "debugging")]
fn spawn_test_menu(
	mut cmds: Commands,
	mut meshes: ResMut<Assets<Mesh>>,
	mut mats: ResMut<Assets<UiMat>>,
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

	let border_mat = mats.add(UiMat {
		base: Matter {
			base: default(),
			extension: DistanceDither::ui(),
		},
		extension: default(),
	});
	let size = Vec3::new(16.0, 16.0, 9.0);
	let mut faces = [Entity::PLACEHOLDER; 6];
	for (i, transform) in CuboidFaces::origins(size + Vec3::ONE)
		.into_iter()
		.enumerate()
	{
		faces[i] = entity_tree!(cmds;
			( // Face container
				WidgetBundle {
					shape: WidgetShape(SharedShape::cuboid(6.3, 0.5, 3.0)),
					transform,
					..default()
				},
				LineUpChildren {
					relative_positions: Vec3::NEG_Z * 1.25,
					align: Vec3::ZERO,
				},
				AdjacentWidgets {
					next: Some(FocusTarget::Child(0)),
					directions: vec![(Wedge2d::circle(), FocusTarget::Child(0))],
					..default()
				};
				#children:
					( // "Testing..." text
						Text3dBundle::<UiMat> {
							text_3d: Text3d {
								text: "Testing...".into(),
								..default()
							},
							font: ui_fonts.mono_3d.clone(),
							transform: Transform::from_translation(Vec3::NEG_Y),
							material: mats.add(new_unlit_material()),
							..default()
						},
						AdjacentWidgets::vertical_siblings(),
					),
					( // Test button
						Button3dBundle {
							shape: WidgetShape(SharedShape::capsule(
								Point::new(0.0, -2.5, 0.0),
								Point::new(0.0, 2.5, 0.0),
								0.5,
							)),
							mesh: meshes.add(Capsule3d::new(0.5, 5.0)),
							material: mats.add(UiMat { extension: default(), base: Matter { extension: DistanceDither::ui(), base: Color::ORANGE.into() }}),
							transform: Transform {
								rotation: Quat::from_rotation_z(-std::f32::consts::FRAC_PI_2),
								..default()
							},
							..default()
						},
						AdjacentWidgets::vertical_siblings();
						#children: ( // Test button text
							Text3dBundle::<UiMat> {
								text_3d: Text3d {
									text: "Test button".into(),
									..default()
								},
								font: ui_fonts.mono_3d.clone(),
								transform: Transform {
									translation: Vec3::X,
									rotation: Quat::from_rotation_z(std::f32::consts::FRAC_PI_2),
									..default()
								},
								material: mats.add(new_unlit_material()),
								..default()
							},
						),
					),
				)
		)
		.id();
	}

	let mut test_menu = cmds.spawn((
		Name::new("TestMenu"),
		CuboidPanelBundle {
			panel: CuboidPanel {
				size,
				borders: std::iter::repeat(vec![
					RectBorderDesc {
						dilation: 1.0,
						colors: Some([Color::BLACK; 4].into()),
						material: border_mat.clone(),
						..default()
					},
					RectBorderDesc {
						dilation: -6.0,
						colors: Some([Color::DARK_GRAY; 4].into()),
						material: border_mat,
						..default()
					},
				])
				.collect(),
				..default()
			},
			transform: Transform {
				translation: Vec3::new(6900.0, 4200.0, 0.0),
				..default()
			},
			material: mats.add(UiMat {
				extension: default(),
				base: Matter {
					extension: DistanceDither::ui(),
					base: StandardMaterial {
						base_color: Color::rgba(0.1, 0.1, 0.1, 0.8),
						reflectance: 0.0,
						alpha_mode: AlphaMode::Blend,
						double_sided: true,
						cull_mode: None,
						..default()
					},
				},
			}),
			..default()
		},
		TestMenu { faces },
		Fade::ZERO,
	));
	for face in faces {
		test_menu.add_child(face);
	}
	*spawned = true;
}

#[cfg(feature = "debugging")]
#[derive(Component)]
struct TestMenu {
	faces: [Entity; 6],
}

#[cfg(feature = "debugging")]
fn toggle_test_menu(
	mut cmds: Commands,
	mut q: Query<(Entity, &TestMenu)>,
	mut stack: Query<&mut MenuStack, With<GlobalUi>>,
	mut cam: Query<&mut UiCam, With<GlobalUi>>,
	input: Res<ButtonInput<KeyCode>>,
	mut focus: ResMut<Focus>,
	mut state: ResMut<StateStack<InputState>>,
	mut i: Local<usize>,
) {
	let Ok((id, info)) = q.get_single_mut() else {
		return;
	};
	if input.just_pressed(KeyCode::Period) {
		if *i >= info.faces.len() {
			return;
		}
		let child = info.faces[*i];

		**focus = Some(child);
		match stack.get_single_mut() {
			Ok(mut stack) => {
				if *i == 0 {
					cmds.entity(id).fade_in_secs(0.5);
					state.push(InputState::InMenu);
				}
				stack.push(child);
				*i = usize::min(*i + 1, info.faces.len());
			}
			Err(e) => error!("{e}"),
		};
		match cam.get_single_mut() {
			Ok(mut cam) => {
				cam.focus = Some(child);
			}
			Err(e) => error!("{e}"),
		}
	}
	if input.just_pressed(KeyCode::Comma) {
		match stack.get_single_mut() {
			Ok(mut stack) => {
				if *i == 1 {
					cmds.entity(id).fade_out_secs(0.5);
					**focus = None;
					stack.pop();
					state.pop();
				} else {
					stack.pop();
				}
				*i = i.saturating_sub(1);
				match cam.get_single_mut() {
					Ok(mut cam) => {
						cam.focus = stack.last().copied();
					}
					Err(e) => error!("{e}"),
				}
			}
			Err(e) => error!("{e}"),
		};
	}
}
