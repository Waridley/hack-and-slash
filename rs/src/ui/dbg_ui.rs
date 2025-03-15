use bevy::{
	ecs::{query::QueryFilter, schedule::SystemConfigs},
	prelude::*,
	window::PrimaryWindow,
};
use bevy_inspector_egui::{
	bevy_egui::EguiContext,
	egui,
	egui::{Color32, Ui},
};
use egui_plot::{Legend, Line, Plot, PlotResponse};
use enum_components::WithVariant;

use engine::ui::{AddDebugUi, ToggleDbgUi, UiHovered};

use crate::{
	planet::{
		chunks::{ChunkCenter, LoadedChunks},
		frame::Frame,
		terrain::Ground,
	},
	player::player_entity::Root,
	util::History,
};

pub fn plugin(app: &mut App) -> &mut App {
	app.add_debug_systems(height_under_player.show_with(KeyCode::KeyH))
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
	.run_if(resource_exists::<History<T>>)
}

pub fn plot_component_history<T: Component, Filter: QueryFilter + 'static, const LINES: usize>(
	map_fn: impl FnMut((usize, &T)) -> (f64, [f64; LINES]) + Clone + Send + Sync + 'static,
) -> SystemConfigs {
	(move |mut egui_contexts: Query<&mut EguiContext, With<PrimaryWindow>>,
	       mut ui_hovered: ResMut<UiHovered>,
	       q: Query<(Entity, &History<T>), Filter>| {
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
	players: Query<&GlobalTransform, WithVariant<Root>>,
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
