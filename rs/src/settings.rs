use bevy::core_pipeline::fxaa::Fxaa;
use bevy::prelude::*;
use bevy_pkv::PkvStore;
use serde::{Deserialize, Serialize};

pub fn plugin(app: &mut App) -> &mut App {
	app.init_resource::<Settings>().add_systems(First, load)
}

pub fn load(settings: Res<Settings>, mut cam_q: Query<&mut Fxaa>, mut msaa: ResMut<Msaa>) {
	if settings.is_changed() {
		for mut fxaa in &mut cam_q {
			*msaa = if settings.msaa {
				Msaa::Sample4
			} else {
				Msaa::Off
			};
			fxaa.enabled = settings.fxaa;
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
