use bevy::{core_pipeline::fxaa::Fxaa, prelude::*};
use bevy_pkv::PkvStore;
use serde::{Deserialize, Serialize};

pub fn plugin(app: &mut App) -> &mut App {
	app.init_resource::<Settings>().add_systems(First, load)
}

pub fn load(mut cmds: Commands, settings: Res<Settings>, cam_q: Query<Entity, With<Camera>>) {
	if settings.is_changed() {
		for id in &cam_q {
			let mut cam = cmds.entity(id);
			cam.insert(if settings.msaa {
				Msaa::Sample4
			} else {
				Msaa::Off
			});
			cam.insert(Fxaa {
				enabled: settings.fxaa,
				..default()
			});
		}
	}
}

/// Settings that are specific to a local game client.
/// E.g. graphics settings should not be shared between machines.
#[derive(Resource, Debug, Clone, Serialize, Deserialize)]
pub struct Settings {
	// pub hdr: bool,
	pub msaa: bool,
	pub fxaa: bool,
}

impl FromWorld for Settings {
	fn from_world(world: &mut World) -> Self {
		world.get_resource::<PkvStore>().map_or(
			Self {
				// hdr: true,
				msaa: false,
				fxaa: false,
			},
			|store| {
				// let hdr = store.get("hdr").unwrap_or(true);
				let msaa = store.get("msaa").unwrap_or(false);
				let fxaa = store.get("fxaa").unwrap_or(false);
				Self { msaa, fxaa }
			},
		)
	}
}
