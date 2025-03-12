use std::cmp::Ordering;

use bevy::{prelude::*, window::PrimaryWindow};
use bevy::window::CursorGrabMode;
use bevy_inspector_egui::{
	bevy_egui::EguiContext, egui, egui::Color32, inspector_options::std_options::NumberOptions,
	prelude::*, quick::WorldInspectorPlugin, reflect_inspector::InspectorUi,
};
use bevy_rapier3d::{
	plugin::{RapierConfiguration, TimestepMode},
	prelude::{DebugRenderContext, RapierDebugRenderPlugin},
};
use egui_plot::{Legend, Line, Plot};
use tiny_bail::prelude::r;
use crate::{
	planet::{day_night::DayNightCycle, weather::Weather},
	util::{Average, History},
};

use super::*;

#[derive(Debug)]
pub struct DebugUiPlugin;

impl Plugin for DebugUiPlugin {
	fn build(&self, app: &mut App) {
		#[cfg(feature = "render")]
		app.add_plugins(RapierDebugRenderPlugin::default())
			.add_systems(Update, toggle_physics_wireframes);

		app.add_plugins(
			WorldInspectorPlugin::new().run_if(dbg_window_toggled(false, KeyCode::KeyI)),
		)
		.init_resource::<Fps>()
		.configure_sets(Update, ShowDebugWindows.after(reset_hovered))
		.add_systems(Startup, set_egui_style)
		.add_systems(
			Update,
			(
				keep_cursor_unlocked.run_if(input_toggle_active(false, KeyCode::Backquote)),
				update_fps,
				History::<Fps>::track_resource,
				dbg_fps.run_if(dbg_window_toggled(true, KeyCode::KeyT)),
				(dbg_res::<DayNightCycle>, dbg_res::<Weather>)
					.run_if(dbg_window_toggled(true, KeyCode::KeyN)),
				dbg_single_proxy::<RapierConfiguration, RapierCfgProxy>
					.run_if(dbg_window_toggled(false, KeyCode::KeyR)),
			)
				.chain() // Just to keep display order consistent
				.in_set(ShowDebugWindows),
		);
	}
}

pub fn keep_cursor_unlocked(
	mut windows: Query<&mut Window>,
) {
	for mut window in &mut windows {
		if window.cursor_options.grab_mode != CursorGrabMode::None {
			window.cursor_options.grab_mode = CursorGrabMode::None;
		}
		if !window.cursor_options.visible {
			window.cursor_options.visible = true;
		}
	}
}

pub fn set_egui_style(mut egui_contexts: Query<&mut EguiContext, With<PrimaryWindow>>) {
	egui_contexts.single_mut().get_mut().style_mut(|style| {
		style.visuals.window_fill = Color32::from_black_alpha(128);
	});
}

pub fn dbg_res<T: Resource + Reflect>(
	mut q: Query<&mut EguiContext, With<PrimaryWindow>>,
	type_registry: Res<AppTypeRegistry>,
	mut ui_hovered: ResMut<UiHovered>,
	mut res: ResMut<T>,
) {
	let egui_context = q.get_single_mut();

	let Ok(egui_context) = egui_context else {
		return;
	};
	let mut egui_context = egui_context.clone();

	let mut hovered = false;
	egui::Window::new(std::any::type_name::<T>()).show(egui_context.get_mut(), |ui| {
		let type_registry = &*type_registry.read();
		let mut ctx = default();
		let mut inspector = InspectorUi::new_no_short_circuit(type_registry, &mut ctx);
		inspector.ui_for_reflect(&mut *res, ui);
		hovered |= ui.ui_contains_pointer();
	});
	if hovered != **ui_hovered {
		**ui_hovered |= hovered;
	}
}

pub fn dbg_proxy<T: Resource + From<Proxy>, Proxy: for<'a> From<&'a T> + PartialEq<T> + Reflect>(
	mut q: Query<&mut EguiContext, With<PrimaryWindow>>,
	type_registry: Res<AppTypeRegistry>,
	mut ui_hovered: ResMut<UiHovered>,
	mut res: ResMut<T>,
) {
	let mut proxy = Proxy::from(&*res);

	let egui_context = q.get_single_mut();

	let Ok(egui_context) = egui_context else {
		return;
	};
	let mut egui_context = egui_context.clone();

	let mut hovered = false;
	egui::Window::new(std::any::type_name::<T>().split("::").last().unwrap()).show(
		egui_context.get_mut(),
		|ui| {
			let type_registry = &*type_registry.read();
			let mut ctx = default();
			let mut inspector = InspectorUi::new_no_short_circuit(type_registry, &mut ctx);
			inspector.ui_for_reflect(&mut proxy, ui);
			hovered |= ui.ui_contains_pointer();
		},
	);
	if hovered != **ui_hovered {
		**ui_hovered |= hovered;
	}

	if proxy != *res {
		*res = proxy.into()
	}
}

pub fn dbg_single_proxy<
	T: Component + From<Proxy>,
	Proxy: for<'a> From<&'a T> + PartialEq<T> + Reflect,
>(
	mut q: Query<&mut EguiContext, With<PrimaryWindow>>,
	type_registry: Res<AppTypeRegistry>,
	mut ui_hovered: ResMut<UiHovered>,
	mut single: Query<&mut T>,
) {
	let mut single = r!(single.get_single_mut());
	
	let mut proxy = Proxy::from(&*single);

	let egui_context = q.get_single_mut();

	let Ok(egui_context) = egui_context else {
		return;
	};
	let mut egui_context = egui_context.clone();

	let mut hovered = false;
	egui::Window::new(std::any::type_name::<T>().split("::").last().unwrap()).show(
		egui_context.get_mut(),
		|ui| {
			let type_registry = &*type_registry.read();
			let mut ctx = default();
			let mut inspector = InspectorUi::new_no_short_circuit(type_registry, &mut ctx);
			inspector.ui_for_reflect(&mut proxy, ui);
			hovered |= ui.ui_contains_pointer();
		},
	);
	if hovered != **ui_hovered {
		**ui_hovered |= hovered;
	}

	if proxy != *single {
		*single = proxy.into()
	}
}

#[derive(Resource, Clone, Debug, Default)]
pub struct Fps {
	fps: f64,
	frame_time: f64,
	frame_count: f64,
}

pub fn update_fps(mut res: ResMut<Fps>, diags: Res<DiagnosticsStore>) {
	if let Some(fps) = diags.get_measurement(&FrameTimeDiagnosticsPlugin::FPS) {
		let fps = fps.value;
		let frame_time = diags
			.get_measurement(&FrameTimeDiagnosticsPlugin::FRAME_TIME)
			.expect("FPS exists, FRAME_TIME should, too")
			.value;
		let frame_count = diags
			.get_measurement(&FrameTimeDiagnosticsPlugin::FRAME_COUNT)
			.expect("FPS exists, FRAME_COUNT should, too")
			.value;
		*res = Fps {
			fps,
			frame_time,
			frame_count,
		}
	}
}

pub fn dbg_fps(
	mut fps: ResMut<History<Fps>>,
	mut egui_contexts: Query<&mut EguiContext, With<PrimaryWindow>>,
	type_registry: Res<AppTypeRegistry>,
	mut ui_hovered: ResMut<UiHovered>,
) {
	let mut hovered = false;
	let Ok(mut ctx) = egui_contexts.get_single_mut() else {
		return;
	};
	let ctx = ctx.get_mut();
	ctx.style_mut(|style| {
		style.visuals.window_fill = Color32::from_black_alpha(128);
	});
	egui::Window::new("FPS")
		.default_size([144.0, 0.0])
		.show(ctx, |ui| {
			let type_registry = &*type_registry.read();
			let mut ctx = default();
			let mut inspector = InspectorUi::new_no_short_circuit(type_registry, &mut ctx);
			ui.horizontal(|ui| {
				ui.label("FPS");
				ui.add_space(20.0);
				ui.centered_and_justified(|ui| {
					inspector.ui_for_reflect_readonly(
						&fps.iter().map(|fps| &fps.fps).average::<f64, f64, f64>(),
						ui,
					)
				});
			});
			ui.horizontal(|ui| {
				ui.label("Δt");
				ui.add_space(26.0);
				ui.centered_and_justified(|ui| {
					inspector.ui_for_reflect_readonly(
						&fps.iter()
							.map(|fps| &fps.frame_time)
							.average::<f64, f64, f64>(),
						ui,
					)
				});
			});
			ui.horizontal(|ui| {
				ui.label("Min Δt");
				ui.add_space(1.0);
				ui.centered_and_justified(|ui| {
					inspector.ui_for_reflect_readonly(
						&fps.iter()
							.map(|fps| fps.frame_time)
							.min_by(|l, r| l.partial_cmp(r).unwrap_or(Ordering::Equal))
							.unwrap_or(0.0),
						ui,
					)
				});
			});
			ui.horizontal(|ui| {
				ui.label("Max Δt");
				ui.add_space(1.0);
				ui.centered_and_justified(|ui| {
					inspector.ui_for_reflect_readonly(
						&fps.iter()
							.map(|fps| fps.frame_time)
							.max_by(|l, r| l.partial_cmp(r).unwrap_or(Ordering::Equal))
							.unwrap_or(0.0),
						ui,
					)
				});
			});
			ui.horizontal(|ui| {
				ui.label("Frame");
				ui.add_space(4.0);
				ui.centered_and_justified(|ui| {
					inspector.ui_for_reflect_readonly(&fps.last().frame_count, ui)
				});
			});
			ui.horizontal(|ui| {
				ui.label("Avg. over");
				ui.centered_and_justified(|ui| {
					let mut size = fps.max_size();
					let mut opts = NumberOptions::<usize>::default();
					opts.min = Some(1);
					" frames".clone_into(&mut opts.suffix);
					inspector.ui_for_reflect_with_options(
						&mut size,
						ui,
						egui::Id::new("FPS Average Window"),
						&opts,
					);
					// Will do no work if size doesn't change, and change detection happens every frame for history anyway
					fps.resize(size);
				});
			});
			Plot::new("FPS")
				.legend(Legend::default())
				.allow_boxed_zoom(false)
				.allow_drag(false)
				.allow_scroll(false)
				.show(ui, |ui| {
					let mut t = 0.0;
					ui.line(Line::new(
						fps.iter()
							.map(|fps| {
								let point = [t, fps.fps];
								t += fps.frame_time;
								point
							})
							.collect::<Vec<_>>(),
					))
				});
			hovered |= ui.ui_contains_pointer();
		});

	if **ui_hovered != hovered {
		**ui_hovered |= hovered;
	}
}

pub fn toggle_physics_wireframes(
	mut ctx: ResMut<DebugRenderContext>,
	input: Res<ButtonInput<KeyCode>>,
) {
	if input.just_pressed(KeyCode::KeyP) {
		ctx.enabled = !ctx.enabled
	}
}

#[derive(Reflect, Resource, Default, InspectorOptions)]
#[reflect(Resource, InspectorOptions)]
struct RapierCfgProxy {
	pub gravity: bevy_rapier3d::math::Vect,
	pub physics_pipeline_active: bool,
	pub query_pipeline_active: bool,
	pub scaled_shape_subdivision: u32,
	pub force_update_from_transform_changes: bool,
}

impl From<&RapierConfiguration> for RapierCfgProxy {
	fn from(value: &RapierConfiguration) -> Self {
		Self {
			gravity: value.gravity,
			physics_pipeline_active: value.physics_pipeline_active,
			query_pipeline_active: value.query_pipeline_active,
			scaled_shape_subdivision: value.scaled_shape_subdivision,
			force_update_from_transform_changes: value.force_update_from_transform_changes,
		}
	}
}

impl From<RapierCfgProxy> for RapierConfiguration {
	fn from(value: RapierCfgProxy) -> Self {
		Self {
			gravity: value.gravity,
			physics_pipeline_active: value.physics_pipeline_active,
			query_pipeline_active: value.query_pipeline_active,
			scaled_shape_subdivision: value.scaled_shape_subdivision,
			force_update_from_transform_changes: value.force_update_from_transform_changes,
		}
	}
}

impl PartialEq<RapierConfiguration> for RapierCfgProxy {
	fn eq(&self, other: &RapierConfiguration) -> bool {
		self.gravity == other.gravity
			&& self.physics_pipeline_active == other.physics_pipeline_active
			&& self.query_pipeline_active == other.query_pipeline_active
			&& self.scaled_shape_subdivision == other.scaled_shape_subdivision
			&& self.force_update_from_transform_changes == other.force_update_from_transform_changes
	}
}

#[derive(Reflect, PartialEq, InspectorOptions)]
#[reflect(InspectorOptions)]
pub enum TimestepModeProxy {
	Fixed {
		#[inspector(min = 0.001)]
		dt: f32,
		#[inspector(min = 1)]
		substeps: usize,
	},
	Variable {
		#[inspector(min = 0.001)]
		max_dt: f32,
		time_scale: f32,
		#[inspector(min = 1)]
		substeps: usize,
	},
	Interpolated {
		#[inspector(min = 0.001)]
		dt: f32,
		time_scale: f32,
		#[inspector(min = 1)]
		substeps: usize,
	},
}

impl Default for TimestepModeProxy {
	fn default() -> Self {
		TimestepModeProxy::Variable {
			max_dt: 1.0 / 60.0,
			time_scale: 1.0,
			substeps: 1,
		}
	}
}

impl From<&TimestepMode> for TimestepModeProxy {
	fn from(value: &TimestepMode) -> Self {
		match *value {
			TimestepMode::Fixed { dt, substeps } => Self::Fixed { dt, substeps },
			TimestepMode::Variable {
				max_dt,
				time_scale,
				substeps,
			} => Self::Variable {
				max_dt,
				time_scale,
				substeps,
			},
			TimestepMode::Interpolated {
				dt,
				time_scale,
				substeps,
			} => Self::Interpolated {
				dt,
				time_scale,
				substeps,
			},
		}
	}
}

impl From<TimestepModeProxy> for TimestepMode {
	fn from(value: TimestepModeProxy) -> Self {
		match value {
			TimestepModeProxy::Fixed { dt, substeps } => Self::Fixed { dt, substeps },
			TimestepModeProxy::Variable {
				max_dt,
				time_scale,
				substeps,
			} => Self::Variable {
				max_dt,
				time_scale,
				substeps,
			},
			TimestepModeProxy::Interpolated {
				dt,
				time_scale,
				substeps,
			} => Self::Interpolated {
				dt,
				time_scale,
				substeps,
			},
		}
	}
}
