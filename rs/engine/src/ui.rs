use crate::ui::widgets::Font3d;
use bevy::{
	asset::{io::Reader, AssetLoader, BoxedFuture, LoadContext},
	diagnostic::{DiagnosticsStore, FrameTimeDiagnosticsPlugin},
	ecs::{query::QuerySingleError, schedule::SystemConfigs},
	input::common_conditions::input_toggle_active,
	prelude::*,
	render::view::{Layer, RenderLayers},
	ui::FocusPolicy,
};
use futures_lite::AsyncReadExt;
use meshtext::MeshGenerator;
use std::ops::{Add, Shl};

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
			.init_asset::<Font3d>()
			.register_asset_loader(Font3dLoader)
			.add_systems(Startup, setup)
			.add_systems(Update, (reset_hovered, show_fps))
			.add_systems(PostUpdate, layout::apply_constraints);
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
	cmds.spawn((
		Camera3dBundle {
			camera: Camera {
				hdr: true,
				order: GLOBAL_UI_LAYER as _,
				clear_color: ClearColorConfig::None,
				..default()
			},
			transform: Transform {
				translation: Vec3::new(0.0, -32.0, 8.0),
				rotation: Quat::from_rotation_arc(
					// default forward
					Vec3::NEG_Z,
					// desired forward
					Vec3::new(0.0, 4.0, -1.0).normalize(),
				),
				..default()
			},
			..default()
		},
		GLOBAL_UI_RENDER_LAYERS,
		GlobalUiCam,
	));
	cmds.spawn((
		DirectionalLightBundle {
			transform: Transform::from_rotation(Quat::from_rotation_arc(
				Vec3::NEG_Z,
				Vec3::new(0.0, 1.0, -0.01).normalize(),
			)),
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

impl AssetLoader for Font3dLoader {
	type Asset = Font3d;
	type Settings = ();
	type Error = std::io::Error;

	fn load<'a>(
		&'a self,
		reader: &'a mut Reader,
		settings: &'a Self::Settings,
		load_context: &'a mut LoadContext,
	) -> BoxedFuture<'a, Result<Self::Asset, Self::Error>> {
		Box::pin(async move {
			let mut buf = vec![];
			reader.read_to_end(&mut buf).await?;
			Ok(Font3d(MeshGenerator::new(buf)))
		})
	}

	fn extensions(&self) -> &[&str] {
		&["ttf"]
	}
}

/// Marker for the camera entity that displays UI for the game not tied to a specific player.
#[derive(Component)]
pub struct GlobalUiCam;
