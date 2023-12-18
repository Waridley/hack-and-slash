use crate::util::{Average, History};
use bevy::{
	diagnostic::{DiagnosticsStore, FrameTimeDiagnosticsPlugin},
	input::common_conditions::input_toggle_active,
	prelude::*,
};
use bevy::window::PrimaryWindow;
use bevy_inspector_egui::{egui, inspector_options::std_options::NumberOptions, quick::WorldInspectorPlugin, reflect_inspector::InspectorUi};
use bevy_inspector_egui::bevy_egui::EguiContext;
use bevy_rapier3d::prelude::{DebugRenderContext, RapierDebugRenderPlugin};
use crate::planet::day_night::DayNightCycle;
use crate::ui::UiHovered;

pub fn plugin(app: &mut App) -> &mut App {
	#[cfg(feature = "render")]
	app.add_plugins(RapierDebugRenderPlugin::default())
		.add_systems(Update, toggle_physics_wireframes);

	app
		.add_plugins((
			WorldInspectorPlugin::new().run_if(dbg_window_toggled(false, KeyCode::I)),
		))
		.init_resource::<Fps>()
		.insert_resource(History::<Fps>::new(240))
		.add_systems(
			Update,
			(
				reset_hovered,
				(
					update_fps,
					History::<Fps>::track_resource.after(update_fps).before(dbg_fps),
					dbg_fps.run_if(dbg_window_toggled(true, KeyCode::F)),
					dbg_res::<DayNightCycle>
						.run_if(dbg_window_toggled(true, KeyCode::N))
						.after(dbg_fps),
				).after(reset_hovered).before(crate::player::input::grab_mouse),
			)
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

pub fn dbg_fps(
	mut fps: ResMut<History<Fps>>,
	mut egui_contexts: Query<&mut EguiContext, With<PrimaryWindow>>,
	type_registry: Res<AppTypeRegistry>,
	mut ui_hovered: ResMut<UiHovered>,
) {
	let mut hovered = false;
	egui::Window::new("FPS")
		.fixed_size([144.0, 0.0])
		.show(
			egui_contexts.single_mut().get_mut(),
			|ui| {
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
				hovered |= ui.ui_contains_pointer();
			},
		);
	
	if **ui_hovered != hovered {
		**ui_hovered |= hovered;
	}
}

fn dbg_res<T: Resource + Reflect>(
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
	egui::Window::new(std::any::type_name::<T>().split("::").last().unwrap())
		.show(egui_context.get_mut(), |ui| {
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
