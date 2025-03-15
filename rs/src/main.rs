#![cfg_attr(
	all(not(debug_assertions), target_os = "windows"),
	windows_subsystem = "windows"
)]

use bevy::{prelude::*, window::PresentMode};
pub(crate) use sond_has::*;

#[bevy_main]
pub fn main() -> AppExit {
	let mut app = App::new();
	let default_plugins = DefaultPlugins
		.set(WindowPlugin {
			primary_window: Some(Window {
				title: "Sonday Hack-and-Slash Game".to_string(),
				resizable: true,
				canvas: Some("#game_canvas".into()),
				present_mode: PresentMode::AutoNoVsync,
				..default()
			}),
			..default()
		})
		.set(AssetPlugin {
			mode: AssetMode::Processed,
			..default()
		});

	#[cfg(not(feature = "render"))]
	let default_plugins = {
		default_plugins
			.disable::<bevy::render::RenderPlugin>()
			.disable::<bevy::core_pipeline::CorePipelinePlugin>()
			.disable::<bevy::sprite::SpritePlugin>()
			.disable::<bevy::ui::UiPlugin>()
			.disable::<bevy::pbr::PbrPlugin>()
			.disable::<bevy::gizmos::GizmoPlugin>()
			.disable::<bevy::window::WindowPlugin>()
			.disable::<bevy::winit::WinitPlugin>()
			.disable::<bevy::text::TextPlugin>()
	};

	app.add_plugins(default_plugins);

	#[cfg(not(feature = "render"))]
	{
		app.init_asset::<bevy::render::render_resource::Shader>()
			.init_asset_loader::<bevy::render::render_resource::ShaderLoader>()
			.init_asset::<bevy::render::mesh::Mesh>()
			.init_asset::<bevy::text::Font>()
			.add_event::<bevy::window::WindowEvent>()
			.add_event::<bevy::window::CursorMoved>();
	}

	app.add_plugins(GamePlugin);

	#[cfg(feature = "import_assets")]
	{
		#[derive(Resource)]
		struct ExitTimer(Timer);

		fn exit_after_delay(
			t: Res<Time>,
			mut timer: ResMut<ExitTimer>,
			mut exit_events: EventWriter<AppExit>,
		) {
			// TODO: Is there a way to just check if all assets are imported?
			// Or is it guaranteed to happen before exit?
			if timer.0.just_finished() {
				exit_events.send(AppExit::Success);
			} else {
				timer.0.tick(t.delta());
			}
		}

		if std::env::args().any(|arg| arg == "--just-import-assets") {
			app.insert_resource(ExitTimer(Timer::from_seconds(2.0, TimerMode::Once)))
				.add_systems(Last, exit_after_delay);
		}
	}

	app.run()
}
