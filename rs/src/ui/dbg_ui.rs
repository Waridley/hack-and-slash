use crate::{
	planet::{
		chunks::{ChunkCenter, LoadedChunks},
		day_night::DayNightCycle,
		frame::Frame,
		terrain::Ground,
		weather::Weather,
	},
	ui::UiHovered,
	util::{Average, History},
};
use bevy::{
	diagnostic::{DiagnosticsStore, FrameTimeDiagnosticsPlugin},
	ecs::{query::ReadOnlyWorldQuery, schedule::SystemConfigs},
	input::common_conditions::input_toggle_active,
	prelude::*,
	window::PrimaryWindow,
};
use bevy_inspector_egui::{
	bevy_egui::EguiContext,
	egui,
	egui::{Color32, Ui},
	inspector_options::std_options::NumberOptions,
	prelude::*,
	quick::WorldInspectorPlugin,
	reflect_inspector::InspectorUi,
};
use bevy_rapier3d::{
	plugin::{RapierConfiguration, TimestepMode},
	prelude::{DebugRenderContext, RapierDebugRenderPlugin},
};
use egui_plot::{Legend, Line, Plot, PlotResponse};
use std::cmp::Ordering;

pub fn plugin(app: &mut App) -> &mut App {
	#[cfg(feature = "render")]
	app.add_plugins(RapierDebugRenderPlugin::default())
		.add_systems(Update, toggle_physics_wireframes);

	app.add_plugins((WorldInspectorPlugin::new().run_if(dbg_window_toggled(false, KeyCode::I)),))
		.init_resource::<Fps>()
		.add_systems(Startup, set_egui_style)
		.add_systems(
			Update,
			(
				reset_hovered,
				(
					// Use `.after(...)` just to keep order consistent
					update_fps,
					History::<Fps>::track_resource
						.after(update_fps)
						.before(dbg_fps),
					dbg_fps.run_if(dbg_window_toggled(true, KeyCode::T)),
					(dbg_res::<DayNightCycle>, dbg_res::<Weather>)
						.run_if(dbg_window_toggled(true, KeyCode::N))
						.after(dbg_fps),
					dbg_proxy::<RapierConfiguration, RapierCfgProxy>
						.run_if(dbg_window_toggled(false, KeyCode::R)),
				)
					.after(reset_hovered)
					.before(crate::player::input::grab_mouse),
				height_under_player.run_if(dbg_window_toggled(true, KeyCode::H)),
			),
		)
}

fn dbg_window_toggled(default: bool, code: KeyCode) -> impl Condition<()> {
	input_toggle_active(false, KeyCode::Grave).and_then(input_toggle_active(default, code))
}

fn reset_hovered(mut ui_hovered: ResMut<UiHovered>) {
	if **ui_hovered {
		**ui_hovered = false;
	}
}

pub fn set_egui_style(mut egui_contexts: Query<&mut EguiContext, With<PrimaryWindow>>) {
	egui_contexts.single_mut().get_mut().style_mut(|style| {
		style.visuals.window_fill = Color32::from_black_alpha(128);
	});
}

pub fn dbg_fps(
	mut fps: ResMut<History<Fps>>,
	mut egui_contexts: Query<&mut EguiContext, With<PrimaryWindow>>,
	type_registry: Res<AppTypeRegistry>,
	mut ui_hovered: ResMut<UiHovered>,
) {
	let mut hovered = false;
	let mut ctx = egui_contexts.single_mut();
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
					opts.suffix = " frames".to_owned();
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

#[derive(Reflect, Resource, Default, InspectorOptions)]
#[reflect(Resource, InspectorOptions)]
struct RapierCfgProxy {
	pub gravity: bevy_rapier3d::math::Vect,
	pub physics_pipeline_active: bool,
	pub query_pipeline_active: bool,
	pub timestep_mode: TimestepModeProxy,
	pub scaled_shape_subdivision: u32,
	pub force_update_from_transform_changes: bool,
}

impl From<&RapierConfiguration> for RapierCfgProxy {
	fn from(value: &RapierConfiguration) -> Self {
		Self {
			gravity: value.gravity,
			physics_pipeline_active: value.physics_pipeline_active,
			query_pipeline_active: value.query_pipeline_active,
			timestep_mode: (&value.timestep_mode).into(),
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
			timestep_mode: value.timestep_mode.into(),
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
			&& self.timestep_mode == (&other.timestep_mode).into()
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

#[derive(Resource, Clone, Debug, Default)]
pub struct Fps {
	fps: f64,
	frame_time: f64,
	frame_count: f64,
}

pub fn update_fps(mut res: ResMut<Fps>, diags: Res<DiagnosticsStore>) {
	if let Some(fps) = diags.get_measurement(FrameTimeDiagnosticsPlugin::FPS) {
		let fps = fps.value;
		let frame_time = diags
			.get_measurement(FrameTimeDiagnosticsPlugin::FRAME_TIME)
			.expect("FPS exists, FRAME_TIME should, too")
			.value;
		let frame_count = diags
			.get_measurement(FrameTimeDiagnosticsPlugin::FRAME_COUNT)
			.expect("FPS exists, FRAME_COUNT should, too")
			.value;
		*res = Fps {
			fps,
			frame_time,
			frame_count,
		}
	}
}

pub fn toggle_physics_wireframes(mut ctx: ResMut<DebugRenderContext>, input: Res<Input<KeyCode>>) {
	if input.just_pressed(KeyCode::P) {
		ctx.enabled = !ctx.enabled
	}
}

pub fn plot_res_history<T: Resource, const LINES: usize>(
	map_fn: impl FnMut((usize, &T)) -> (f64, [f64; LINES]) + Clone + Send + Sync + 'static,
) -> SystemConfigs {
	(move |mut egui_contexts: Query<&mut EguiContext, With<PrimaryWindow>>,
	       mut ui_hovered: ResMut<UiHovered>,
	       history: Res<History<T>>| {
		let mut ctx = egui_contexts.single_mut();
		let ctx = ctx.get_mut();
		let map_fn = map_fn.clone();
		egui::Window::new(std::any::type_name::<T>().split("::").last().unwrap())
			.default_size([f32::min(720.0, history.max_size() as f32 * 2.0), 200.0])
			.show(ctx, |ui| {
				plot_history_mapped(ui, &*history, map_fn);
				let hovered = ui.ui_contains_pointer();
				if **ui_hovered != hovered {
					**ui_hovered |= hovered;
				}
			});
	})
	.run_if(resource_exists::<History<T>>())
}

pub fn plot_component_history<
	T: Component,
	QueryFilter: ReadOnlyWorldQuery + 'static,
	const LINES: usize,
>(
	map_fn: impl FnMut((usize, &T)) -> (f64, [f64; LINES]) + Clone + Send + Sync + 'static,
) -> SystemConfigs {
	(move |mut egui_contexts: Query<&mut EguiContext, With<PrimaryWindow>>,
	       mut ui_hovered: ResMut<UiHovered>,
	       q: Query<(Entity, &History<T>), QueryFilter>| {
		let mut hovered = false;
		let mut ctx = egui_contexts.single_mut();
		let ctx = ctx.get_mut();
		for (id, history) in &q {
			let map_fn = map_fn.clone();
			egui::Window::new(format!(
				"{} {id:?}",
				std::any::type_name::<T>().split("::").last().unwrap()
			))
			.default_size([f32::min(720.0, history.max_size() as f32 * 2.0), 200.0])
			.show(ctx, |ui| {
				plot_history_mapped(ui, history, map_fn);
				hovered |= ui.ui_contains_pointer();
			});
		}
		if **ui_hovered != hovered {
			**ui_hovered |= hovered;
		}
	})
	.into_configs()
}

pub fn plot_history_mapped<T, const LINES: usize>(
	ui: &mut Ui,
	history: &History<T>,
	map_fn: impl FnMut((usize, &T)) -> (f64, [f64; LINES]),
) -> PlotResponse<()> {
	Plot::new(std::any::type_name::<T>())
		.legend(Legend::default())
		.allow_boxed_zoom(false)
		.allow_drag(false)
		.allow_scroll(false)
		.show(ui, |ui| {
			let outputs = history.iter().enumerate().map(map_fn).collect::<Vec<_>>();
			for i in 0..LINES {
				ui.line(Line::new(
					outputs
						.iter()
						.map(|outputs| [outputs.0, outputs.1[i]])
						.collect::<Vec<_>>(),
				));
			}
		})
}

pub fn height_under_player(
	mut egui_contexts: Query<&mut EguiContext, With<PrimaryWindow>>,
	players: Query<&GlobalTransform, enum_components::ERef<crate::player::player_entity::Root>>,
	chunks: Query<(&ChunkCenter, &Ground)>,
	frame: Res<Frame>,
	loaded_chunks: Res<LoadedChunks>,
) {
	let mut ctx = egui_contexts.single_mut();
	let ctx = ctx.get_mut();
	ctx.style_mut(|style| {
		style.visuals.window_fill = Color32::from_black_alpha(128);
	});
	egui::Window::new("Terrain Height").show(ctx, |ui| {
		for global in &players {
			let pos = frame.planet_coords_of(global.translation().xy());
			ui.label(format!("At: {:.2?}", pos));
			ui.label(format!(
				"Height: {:.2?}",
				loaded_chunks.height_at(pos, &chunks)
			));
		}
	});
}
