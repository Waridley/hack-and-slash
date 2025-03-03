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
		focus::{AdjacentWidgets, FocusTarget},
		layout::LineUpChildren,
		widgets::{
			draw_widget_shape_gizmos, CuboidFaces, CuboidPanel, CuboidPanelBundle, Text3d, Text3dBundle, WidgetShape,
		},
	},
	util::{Diff, LerpSlerp, StateStack},
};
use leafwing_input_manager::{prelude::*, Actionlike};
use rapier3d::{geometry::SharedShape, math::Point};
use serde::{Deserialize, Serialize};
use std::{
	collections::VecDeque,
	f64::consts::TAU,
	fmt::Formatter,
	ops::{Add, ControlFlow::Break, Mul},
};
use bevy::{
	color::palettes::css::ORANGE,
	color::palettes::basic::{AQUA, BLUE, FUCHSIA, GREEN, RED, WHITE, YELLOW},
	a11y::Focus,
	core_pipeline::fxaa::Fxaa,
	diagnostic::{DiagnosticsStore, FrameTimeDiagnosticsPlugin},
	ecs::{
		query::QuerySingleError,
		schedule::SystemConfigs,
		system::EntityCommands,
	},
	input::common_conditions::input_toggle_active,
	prelude::*,
	render::{
		camera::Viewport,
		view::{Layer, RenderLayers, VisibilitySystems::CheckVisibility},
	},
	ui::FocusPolicy,
	core_pipeline::experimental::taa::TemporalAntiAliasing
};
use smallvec::smallvec;

pub mod a11y;
#[cfg(feature = "debugging")]
pub mod dbg;
pub mod focus;
pub mod layout;
pub mod mouse;
pub mod text;
pub mod widgets;

pub const GLOBAL_UI_LAYER: Layer = 31 as Layer;
pub const GLOBAL_UI_RENDER_LAYERS: RenderLayers = RenderLayers::layer(GLOBAL_UI_LAYER);

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
				(
					draw_widget_shape_gizmos::<GLOBAL_UI_LAYER>,
					focus::highlight_focus::<GLOBAL_UI_LAYER>,
				)
					.chain()
					.run_if(input_toggle_active(false, KeyCode::KeyG))
					.after(CheckVisibility),
			)
			.insert_gizmo_config(
				widgets::WidgetGizmos::<GLOBAL_UI_LAYER>,
				GizmoConfig {
					render_layers: GLOBAL_UI_RENDER_LAYERS,
					..default()
				},
			)
			.insert_gizmo_config(
				focus::FocusGizmos::<GLOBAL_UI_LAYER>,
				GizmoConfig {
					line_width: 6.0,
					render_layers: GLOBAL_UI_RENDER_LAYERS,
					..default()
				},
			);

		app.add_plugins((
			InputManagerPlugin::<UiAction>::default(),
			AnimationPlugin::<Fade>::default(),
			widgets::borders::WidgetBordersPlugin,
		))
		.register_type::<MenuStack>()
		.register_type::<UiCam>()
		.register_type::<Fade>()
		.register_type::<CuboidPanel>()
		.register_type::<CylinderPanel>()
		.register_type::<DitherFade>()
		.register_type::<Text3d>()
		.init_resource::<ActionState<UiAction>>()
		.insert_resource(UiAction::default_mappings())
		.init_resource::<UiHovered>()
		.init_resource::<TextMeshCache>()
		.init_resource::<Tessellator>()
		.init_resource::<mouse::MouseLayers>()
		.add_systems(Startup, setup)
		.add_systems(
			Update,
			(
				show_fps,
				mouse::mouse_picks_focus,
				focus::handle_focus_actions,
			),
		)
		.add_systems(
			PostUpdate,
			(
				layout::apply_constraints,
				layout::RadialChildren::apply,
				CuboidContainer::sync,
				ExpandToFitChildren::apply::<CuboidPanel>,
				ExpandToFitChildren::apply::<CylinderPanel>,
				ExpandToFitChildren::apply::<CuboidContainer>,
				InteractHandlers::system,
			),
		)
		.add_systems(
			Last,
			(
				hide_orphaned_popups,
				propagate_fade::<UiMat>.before(Fade::hide_faded_out),
				Fade::hide_faded_out,
				anchor_follow_menu,
				CuboidPanel::sync,
				CylinderPanel::sync,
				Text3d::sync,
			),
		);
	}

	fn finish(&self, app: &mut App) {
		let srv = app.world().resource::<AssetServer>();
		let mono = srv.load("ui/fonts/Noto_Sans_Mono/static/NotoSansMono-Bold.ttf");
		app.insert_resource(UiFonts { mono });
		app.world_mut().resource_mut::<Assets<_>>().insert(
			Handle::weak_from_u128(widgets::UNLIT_MATERIAL_ID).id(),
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
		Name::new(format!("CamAnchor_{}", layer)),
		identifier.clone(),
		CamAnchor,
		layers.clone(),
	))
	.with_children(|cmds| {
		cmds.spawn((
			Name::new(format!("PopupsRoot_{}", layer)),
			identifier.clone(),
			PopupsRoot,
			Transform {
				translation: Vec3::NEG_Y * 10.0,
				..default()
			},
			layers.clone(),
		));

		let mut cam = cmds.spawn((
			Name::new(format!("UICam_{}", layer)),
			identifier,
			UiCam::default(),
			Camera {
				hdr: true,
				order: layer as _,
				clear_color: ClearColorConfig::None,
				viewport,
				..default()
			},
			Projection::Perspective(PerspectiveProjection {
				fov: std::f32::consts::FRAC_PI_3,
				..default()
			}),
			Transform {
				translation: cam_pos,
				rotation: Quat::from_rotation_arc(
					// default forward
					Vec3::NEG_Z,
					// desired forward
					-cam_pos.normalize(),
				),
				..default()
			},
			TemporalAntiAliasing::default(),
			Fxaa::default(),
			layers.clone(),
		));

		cam.start_animation::<Transform>(cam_idle_animation(animation_time_offset));

		// Lights add together to white in front, but tint the sides of UI elements
		let green_light_pos = cam_pos + Vec3::new(2.0, 0.0, 4.0);
		cmds.spawn((
			SpotLight {
				range: 256.0,
				radius: 128.0,
				color: Color::linear_rgb(0.5, 0.75, 0.125),
				intensity: 160_000_000.0,
				inner_angle: std::f32::consts::FRAC_PI_8 * 7.0,
				outer_angle: std::f32::consts::FRAC_PI_8 * 7.5,
				..default()
			},
			Transform {
				translation: green_light_pos * 2.0,
				rotation: Quat::from_rotation_arc(
					Vec3::NEG_Z,
					(-green_light_pos).normalize(),
				),
				..default()
			},
			layers.clone(),
		));
		let blue_light_pos = cam_pos + Vec3::new(-2.0, 0.0, 4.0);
		cmds.spawn((
			SpotLight {
				range: 256.0,
				radius: 128.0,
				color: Color::linear_rgb(0.5, 0.25, 0.875),
				intensity: 160_000_000.0,
				inner_angle: std::f32::consts::FRAC_PI_8 * 7.0,
				outer_angle: std::f32::consts::FRAC_PI_8 * 7.5,
				..default()
			},
			Transform {
				translation: blue_light_pos * 2.0,
				rotation: Quat::from_rotation_arc(
					Vec3::NEG_Z,
					(-blue_light_pos).normalize(),
				),
				..default()
			},
			layers.clone(),
		));

		cmds.spawn((
			SpotLight {
				range: 256.0,
				radius: 128.0,
				intensity: 2_000_000.0,
				inner_angle: std::f32::consts::FRAC_PI_8 * 7.0,
				outer_angle: std::f32::consts::FRAC_PI_8 * 7.5,
				color: Color::linear_rgb(1.0, 0.125, 1.0),
				..default()
			},
			Transform {
				translation: Vec3::NEG_Z * 64.0,
				rotation: Quat::from_rotation_arc(Vec3::NEG_Z, Vec3::Z),
				..default()
			},
			layers.clone(),
		));
	});
}

/// Marker for entities that belong to the global UI (not tied to a player).
#[derive(Component, Reflect, Copy, Clone, Debug)]
#[reflect(Component)]
pub struct GlobalUi;

/// Component for any UI camera entity -- both per-player UI and global UI.
#[derive(Component, Default, Copy, Clone, Debug, Reflect)]
#[require(Camera3d)]
#[reflect(Component)]
pub struct UiCam;

/// Add this component to the parent of each UI camera
#[derive(Component, Reflect, Copy, Clone, Debug)]
#[require(Transform, Visibility, MenuStack, PrevFocus)]
pub struct CamAnchor;

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
	#[actionlike(DualAxis)]
	MoveCursor,
	FocusNext,
	FocusPrev,
	NextTab, // TODO: Should these be an Axis or even DualAxis instead?
	PrevTab,
	#[actionlike(Axis)]
	Zoom,
	ResetZoom,
	#[actionlike(DualAxis)]
	Pan,
	ResetPan,
}

impl UiAction {
	pub const ALL: [Self; 13] = [
		Self::Ok,
		Self::Opt1,
		Self::Opt2,
		Self::Back,
		Self::MoveCursor,
		Self::FocusNext,
		Self::FocusPrev,
		Self::NextTab,
		Self::PrevTab,
		Self::Zoom,
		Self::ResetZoom,
		Self::Pan,
		Self::ResetPan,
	];
}

impl ActionExt for UiAction {
	fn display_name(&self) -> &'static str {
		match self {
			Self::Ok => "Ok",
			Self::Opt1 => "Option 1",
			Self::Opt2 => "Option 2",
			Self::Back => "Back",
			Self::MoveCursor => "Move Cursor",
			Self::FocusNext => "Next Item",
			Self::FocusPrev => "Previous Item",
			Self::NextTab => "Next Tab",
			Self::PrevTab => "Previous Tab",
			Self::Zoom => "Zoom",
			Self::ResetZoom => "Reset Zoom",
			Self::Pan => "Pan",
			Self::ResetPan => "Reset Pan",
		}
	}

	fn default_mappings() -> InputMap<Self> {
		use KeyCode::*;
		use UiAction::*;
		InputMap::new([
			(Ok, Space),
			(Ok, Enter),
			(Opt1, KeyE),
			(Opt2, KeyQ),
			(Back, Escape),
			(Back, Backspace),
			(FocusNext, Tab),
			(NextTab, KeyX),
			(PrevTab, KeyZ),
			(ResetZoom, Digit0),
			(ResetPan, Digit0.into()),
		])
			.with_multiple([
				(Ok, MouseButton::Left),
				(Opt1, MouseButton::Right),
				(ResetZoom, MouseButton::Middle),
				(ResetPan, MouseButton::Middle),
			])
			.with_dual_axis(MoveCursor, VirtualDPad::wasd())
			.with_dual_axis(MoveCursor, VirtualDPad::arrow_keys())
			.with_multiple([
				(FocusNext, MouseScrollDirection::DOWN),
				(FocusNext, MouseScrollDirection::RIGHT),
				(FocusPrev, MouseScrollDirection::UP),
				(FocusPrev, MouseScrollDirection::LEFT),
			])
			.with(FocusPrev, ButtonlikeChord::modified(ModifierKey::Shift, Tab))
			.with_axis(Zoom, AxislikeChord::new(ModifierKey::Control, MouseScrollAxis::Y))
			.with_axis(Zoom, VirtualAxis::new(Minus, Equal))
			.with_dual_axis(Pan, DualAxislikeChord::new(MouseButton::Middle, MouseMove::default()))
			.with_multiple([
				(Ok, GamepadButton::South),
				(Opt1, GamepadButton::West),
				(Opt2, GamepadButton::North),
				(Back, GamepadButton::East),
				(NextTab, GamepadButton::RightTrigger),
				(PrevTab, GamepadButton::LeftTrigger),
				(ResetPan, GamepadButton::RightThumb),
			])
			.with_dual_axis(MoveCursor, VirtualDPad::dpad())
			.with_dual_axis(MoveCursor, GamepadStick::LEFT)
			.with_axis(
				Zoom,
				AxislikeChord::new(
					GamepadButton::LeftTrigger2,
					GamepadControlAxis::new(GamepadAxis::RightStickY).with_processor(AxisProcessor::DeadZone(AxisDeadZone::symmetric(0.1))),
				),
			)
			.with(
				ResetZoom,
				ButtonlikeChord::new([
					GamepadButton::LeftTrigger2,
					GamepadButton::RightThumb,
				]),
			)
			.with_dual_axis(Pan, GamepadStick::RIGHT)
	}

	fn all() -> impl Iterator<Item = Self> {
		Self::ALL.into_iter()
	}
}

impl std::fmt::Display for UiAction {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		f.write_str(self.display_name())
	}
}

/// Marker for the parents of all popup menus.
#[derive(Component, Debug, Reflect)]
#[require(Transform, Visibility)]
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
		let dt = t.delta_secs_f64();
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
				.delta_from(&xform)
					* coef;
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
					Text(format!("{val:.2}")),
					TextFont {
						font: ui_fonts.mono.clone(),
						font_size: 24.0,
						..default()
					},
					TextColor(YELLOW.into()),
					Node {
						position_type: PositionType::Absolute,
						right: Val::ZERO,
						..default()
					},
					FocusPolicy::Pass,
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
		text.0 = format!("{val:.2}");
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
	input_toggle_active(false, KeyCode::Backquote).and(input_toggle_active(default, code))
}

pub fn reset_hovered(mut ui_hovered: ResMut<UiHovered>) {
	if **ui_hovered {
		**ui_hovered = false;
	}
}

#[derive(Component, Default, Debug, Reflect, Deref, DerefMut)]
#[reflect(Component)]
pub struct MenuStack(pub Vec<MenuRef>);

impl MenuStack {
	pub fn pop_on_back(layers: RenderLayers, fade_secs: f32) -> InteractHandlers {
		InteractHandlers::on_back(move |cmds| {
			cmds.fade_out_secs(fade_secs);
			let layers = layers.clone();
			cmds.commands().queue(move |world: &mut World| {
				let mut q = world.query::<(&mut MenuStack, &RenderLayers)>();
				let Some((mut stack, _)) = q
					.iter_mut(world)
					.find(|(_, q_layers)| q_layers.intersects(&layers))
				else {
					error!("couldn't find `MenuStack` for {layers:?}");
					return;
				};
				stack.pop();
			});
			Break(())
		})
	}
}

#[derive(Debug, Copy, Clone, Reflect)]
pub struct MenuRef {
	/// The root entity of the menu, usually containing a [Fade] component for fading in and
	/// out the menu as it is opened and closed.
	pub root: Entity,
	/// Entity whose transform `CamAnchor` should follow.
	/// Usually the same as `root`, unless the menu is large enough to scroll, or there is some
	/// other reason the camera should not be anchored at `root`.
	pub cam_target: Entity,
	/// The currently focused widget in this menu.
	pub focus: Entity,
}

impl MenuRef {
	pub const INVALID: Self = Self {
		root: Entity::PLACEHOLDER,
		cam_target: Entity::PLACEHOLDER,
		focus: Entity::PLACEHOLDER,
	};

	pub fn new(root: Entity) -> Self {
		Self {
			root,
			cam_target: root,
			focus: root,
		}
	}
}

pub fn anchor_follow_menu(
	mut q: Query<(&mut Transform, &MenuStack)>,
	nodes: Query<&GlobalTransform, Without<MenuStack>>,
	t: Res<Time>,
) {
	for (mut xform, stack) in &mut q {
		if let Some(node) = stack.last() {
			let target = match nodes.get(node.cam_target) {
				Ok(node) => node,
				Err(e) => {
					error!("{e}");
					continue;
				}
			};

			let new = xform.lerp_slerp(target.compute_transform(), t.delta_secs() * 4.0);
			if *xform != new {
				*xform = new;
			}
		}
	}
}

use crate::{
	anim::{AnimationHandle, DynAnimation},
	draw::{polygon_points, PlanarPolyLine},
	ui::{
		text::Tessellator,
		widgets::{
			new_unlit_material, CuboidContainer, CylinderPanel, InteractHandlers, PanelBundle,
			PrevFocus,
		},
	},
};
#[cfg(feature = "debugging")]
use bevy_inspector_egui::{
	inspector_options::std_options::NumberDisplay::Slider,
	prelude::{InspectorOptions, ReflectInspectorOptions},
};
use layout::ExpandToFitChildren;
use text::{TextMeshCache, UiFonts};
use web_time::Duration;
use crate::input::ActionExt;
use crate::ui::widgets::Node3d;

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

pub fn propagate_fade<M: Material + AsMut<DitherFade>>(
	q: Query<&MeshMaterial3d<M>>,
	roots: Query<(Entity, Ref<Fade>)>,
	children_q: Query<&Children>,
	mut mats: ResMut<Assets<M>>,
) {
	for (root, fade) in &roots {
		if !fade.is_changed() {
			continue;
		}

		let mut set_fade = |handle: &Handle<M>| {
			let Some(mat) = mats.get_mut(handle) else {
				warn!("missing {handle:?}");
				return;
			};
			mat.as_mut().fade = **fade;
		};

		if let Ok(mat) = q.get(root) {
			set_fade(&mat.0)
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
		self.start_animation(move |id, _fade, t, mut ctrl| {
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
		self.start_animation(move |id, _fade, t, mut ctrl| {
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

impl FadeCommands for EntityWorldMut<'_> {
	fn fade_in(&mut self, duration: Duration) -> AnimationHandle<DynAnimation<Fade>> {
		let mut elapsed = Duration::ZERO;
		let duration = duration.as_secs_f32();
		self.start_animation(move |id, _fade, t, mut ctrl| {
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
		self.start_animation(move |id, _fade, t, mut ctrl| {
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

#[derive(Debug, Clone, Reflect)]
pub struct UiMatBuilder {
	pub fade: f32,
	pub far_start: f32,
	pub far_end: f32,
	pub near_start: f32,
	pub near_end: f32,
	pub std: StandardMaterial,
}

impl Default for UiMatBuilder {
	fn default() -> Self {
		let DistanceDither {
			far_start,
			far_end,
			near_start,
			near_end,
			..
		} = DistanceDither::ui();
		Self {
			fade: DitherFade::default().fade,
			far_start,
			far_end,
			near_start,
			near_end,
			std: default(),
		}
	}
}

impl UiMatBuilder {
	#[inline]
	pub fn build(self) -> UiMat {
		UiMat {
			base: Matter {
				base: self.std,
				extension: DistanceDither {
					far_start: self.far_start,
					far_end: self.far_end,
					near_start: self.near_start,
					near_end: self.near_end,
					..default()
				},
			},
			extension: DitherFade::new(self.fade),
		}
	}
}

impl From<UiMatBuilder> for UiMat {
	#[inline]
	fn from(value: UiMatBuilder) -> Self {
		value.build()
	}
}

impl<T: Into<StandardMaterial>> From<T> for UiMatBuilder {
	#[inline]
	fn from(value: T) -> Self {
		Self {
			std: value.into(),
			..default()
		}
	}
}

#[cfg(feature = "debugging")]
fn spawn_test_menu(
	mut cmds: Commands,
	mut meshes: ResMut<Assets<Mesh>>,
	mut mats: ResMut<Assets<UiMat>>,
	ui_fonts: Res<UiFonts>,
	mut events: EventReader<AssetEvent<Font>>,
	mut spawned: Local<bool>,
) {
	// TODO: Use bevy_asset_loader
	if *spawned {
		return;
	};
	let mut loaded = false;
	for e in events.read() {
		if let AssetEvent::Added { id } = e {
			if *id == ui_fonts.mono.id() {
				loaded = true;
			}
		}
	}
	if !loaded {
		return;
	};

	let border_mat = mats.add(UiMatBuilder::default());
	let size = Vec3::new(16.0, 16.0, 9.0);
	let mut faces = [Entity::PLACEHOLDER; 6];
	for (i, transform) in CuboidFaces::origins(size + Vec3::ONE)
		.into_iter()
		.enumerate()
	{
		faces[i] = entity_tree!(cmds;
			( // Face container
				(
					WidgetShape {
						shape: SharedShape::cuboid(6.3, 0.5, 3.0),
						..default()
					},
					transform,
					AdjacentWidgets::all(FocusTarget::ChildN(0)),
				);
				#children: [
					( // Border
						Node3d,
						AdjacentWidgets::all(FocusTarget::ChildN(0)),
						Mesh3d(meshes.add(PlanarPolyLine {
							points: polygon_points(6, 6.0, i as f32),
							cross_section: polygon_points(3, 0.25, 0.5),
							colors: smallvec![
								smallvec![WHITE.into()],
								smallvec![RED.into()],
								smallvec![BLUE.into()],
								smallvec![GREEN.into()],
								smallvec![AQUA.into()],
								smallvec![FUCHSIA.into()],
							],
							closed: false,
							..default()
						}.mesh().build().with_duplicated_vertices().with_computed_flat_normals())),
						MeshMaterial3d(border_mat.clone()),
						LineUpChildren {
							relative_positions: Vec3::NEG_Z * 1.25,
							align: Vec3::ZERO,
							..default()
						};
						#children:[
							( // "Testing..." text
								Text3dBundle::<UiMat> {
									text_3d: Text3d {
										text: "Testing...".into(),
										font: ui_fonts.mono.clone(),
										..default()
									},
									material: MeshMaterial3d(mats.add(new_unlit_material())),
									..default()
								},
								Transform::from_translation(Vec3::NEG_Y),
								AdjacentWidgets::vertical_siblings(),
							),
							( // Test button
								PanelBundle {
									shape: WidgetShape { shape: SharedShape::capsule(
										Point::new(0.0, -2.5, 0.0),
										Point::new(0.0, 2.5, 0.0),
										0.5,
									), ..default() },
									mesh: Mesh3d(meshes.add(Capsule3d::new(0.5, 5.0))),
									material: MeshMaterial3d(mats.add(UiMatBuilder::from(Color::from(ORANGE)))),
									..default()
								},
								Transform {
									rotation: Quat::from_rotation_z(-std::f32::consts::FRAC_PI_2),
									..default()
								},
								AdjacentWidgets::vertical_siblings(),
								;
								#children: [( // Test button text
									Text3dBundle::<UiMat> {
										text_3d: Text3d {
											text: "Test button".into(),
											font: ui_fonts.mono.clone(),
											..default()
										},
										material: MeshMaterial3d(mats.add(new_unlit_material())),
										..default()
									},
									Transform {
										translation: Vec3::X,
										rotation: Quat::from_rotation_z(std::f32::consts::FRAC_PI_2),
										..default()
									},
								)]
							),
						]
					),
				]
			)
		)
		.id();
	}

	let mut test_menu = cmds.spawn((
		Name::new("TestMenu"),
		CuboidPanelBundle {
			panel: CuboidPanel { size, ..default() },
			material: MeshMaterial3d(mats.add(UiMatBuilder {
				std: StandardMaterial {
					base_color: Color::linear_rgba(0.1, 0.1, 0.1, 0.8),
					reflectance: 0.0,
					alpha_mode: AlphaMode::Blend,
					double_sided: true,
					cull_mode: None,
					..default()
				},
				..default()
			})),
			..default()
		},
		Transform {
			translation: Vec3::new(6900.0, 4200.0, 0.0),
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
				stack.push(MenuRef::new(child));
				*i = usize::min(*i + 1, info.faces.len());
			}
			Err(e) => error!("{e}"),
		};
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
			}
			Err(e) => error!("{e}"),
		};
	}
}
