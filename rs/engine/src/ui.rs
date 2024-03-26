use bevy::{
	diagnostic::{DiagnosticsStore, FrameTimeDiagnosticsPlugin},
	ecs::{query::QuerySingleError, schedule::SystemConfigs},
	input::common_conditions::input_toggle_active,
	prelude::*,
	ui::FocusPolicy,
};

#[cfg(feature = "debugging")]
pub mod dbg;
pub mod in_map;

pub struct UiPlugin;

impl Plugin for UiPlugin {
	fn build(&self, app: &mut App) {
		#[cfg(feature = "debugging")]
		app.add_plugins(dbg::DebugUiPlugin);

		app.init_resource::<UiHovered>()
			.add_systems(Update, (reset_hovered, show_fps));
	}

	fn finish(&self, app: &mut App) {
		let mono = app
			.world
			.resource::<AssetServer>()
			.load("ui/fonts/KodeMono/static/KodeMono-Bold.ttf");
		app.insert_resource(UiFonts { mono });
	}
}

#[derive(Resource)]
pub struct UiFonts {
	pub mono: Handle<Font>,
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
