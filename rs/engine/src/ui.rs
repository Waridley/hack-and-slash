use crate::{
	anim::{ComponentDelta, StartAnimation},
	ui::widgets::{Button3d, Font3d, WidgetShape},
	util::{Diff, Prev},
};
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
use meshtext::{MeshGenerator, QualitySettings};
use serde::{Deserialize, Serialize};
use std::f64::consts::TAU;

pub mod a11y;
#[cfg(feature = "debugging")]
pub mod dbg;
pub mod in_map;
pub mod layout;
pub mod widgets;

pub const GLOBAL_UI_LAYER: Layer = (RenderLayers::TOTAL_LAYERS - 1) as Layer;
pub const GLOBAL_UI_RENDER_LAYERS: RenderLayers = RenderLayers::layer(GLOBAL_UI_LAYER);

pub struct UiPlugin;

impl Plugin for UiPlugin {
	fn build(&self, app: &mut App) {
		#[cfg(feature = "debugging")]
		app.add_plugins((dbg::DebugUiPlugin, in_map::detect::DetectBindingPopupPlugin));

		app.init_resource::<UiHovered>()
			.init_resource::<TextMeshCache>()
			.init_asset::<Font3d>()
			.register_asset_loader(Font3dLoader)
			.add_systems(Startup, setup)
			.add_systems(Update, (reset_hovered, show_fps))
			.add_systems(PostUpdate, layout::apply_constraints)
			.add_systems(Last, Prev::<Button3d>::update_component);
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
	let cam_pos = Vec3::new(0.0, -16.0, 0.0);
	let mut global_ui_cam = cmds.spawn((
		Camera3dBundle {
			camera: Camera {
				hdr: true,
				order: GLOBAL_UI_LAYER as _,
				clear_color: ClearColorConfig::None,
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
		GLOBAL_UI_RENDER_LAYERS,
		GlobalUiCam,
		UiCam::default(),
	));

	let mut loop_t = 0.0;
	let _cam_idle = global_ui_cam.start_animation::<Transform>(move |id, xform, t, _| {
		let dt = t.delta_seconds_f64();
		loop_t = (loop_t + dt) % TAU;
		// lemniscate
		let a = 0.32;
		let sin_t = loop_t.sin();
		let cos_t = loop_t.cos();
		let x = (a * cos_t) / (1.0 + (sin_t * sin_t));
		let z = (a * sin_t * cos_t) / (1.0 + (sin_t * sin_t));
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
	});

	// Lights add together to white in front, but tint the sides of UI elements
	cmds.spawn((
		DirectionalLightBundle {
			directional_light: DirectionalLight {
				color: Color::rgb(0.5, 0.75, 0.125),
				..default()
			},
			transform: Transform::from_rotation(Quat::from_rotation_arc(
				Vec3::NEG_Z,
				Vec3::new(-0.05, 1.0, -0.05).normalize(),
			)),
			..default()
		},
		GLOBAL_UI_RENDER_LAYERS,
	));
	cmds.spawn((
		DirectionalLightBundle {
			directional_light: DirectionalLight {
				color: Color::rgb(0.5, 0.25, 0.875),
				..default()
			},
			transform: Transform::from_rotation(Quat::from_rotation_arc(
				Vec3::NEG_Z,
				Vec3::new(0.05, 1.0, -0.05).normalize(),
			)),
			..default()
		},
		GLOBAL_UI_RENDER_LAYERS,
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
		GLOBAL_UI_RENDER_LAYERS,
	));
}

/// Add this component to each entity that should be the root of a player's UI tree.
#[derive(Component, Reflect)]
#[reflect(Component)]
pub struct UiRoot;

#[derive(Component, Reflect)]
#[reflect(Component)]
pub struct GlobalUiLight;

#[derive(Resource)]
pub struct UiFonts {
	pub mono: Handle<Font>,
	pub mono_3d: Handle<Font3d>,
}

/// Keeps track of whether a UI element is hovered over so that clicking
/// does not grab the mouse if so.
#[derive(Resource, Debug, Default, Copy, Clone, Deref, DerefMut, Reflect)]
pub struct UiHovered(bool);

#[derive(SystemSet, Copy, Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ShowDebugWindows;

pub trait AddDebugUi {
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

pub trait ToggleUi<M> {
	/// Debug UI window will be hidden by default, and can be shown by pressing the given
	/// key while the debug interface is visible.
	fn show_with(self, key: KeyCode) -> SystemConfigs;
	/// Debug UI window will be visible by default, and can be hidden by pressing the given
	/// key while the debug interface is visible.
	fn hide_with(self, key: KeyCode) -> SystemConfigs;
}

impl<S: IntoSystemConfigs<M>, M> ToggleUi<M> for S {
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

#[derive(Resource, Default, Clone, Debug, Deref, DerefMut)]
pub struct TextMeshCache(
	pub  HashMap<
		(CowArc<'static, str>, [u32; 16], Handle<Font3d>),
		Option<(Handle<Mesh>, WidgetShape)>,
	>,
);

/// Component for any UI camera entity, player or global.
#[derive(Component, Default)]
pub struct UiCam {
	pub focus: Option<Entity>,
}

/// Marker for the camera entity that displays UI for the game not tied to a specific player.
#[derive(Component)]
pub struct GlobalUiCam;
