use bevy::core_pipeline::fxaa::Fxaa;
use bevy::prelude::*;
use bevy_pkv::PkvStore;
use serde::{Deserialize, Serialize};

pub fn plugin(app: &mut App) -> &mut App {
	app.init_resource::<Settings>()
		.add_system_to_stage(CoreStage::First, load)
}

pub fn load(
	settings: Res<Settings>,
	mut cam_q: Query<(&mut Camera, &mut Fxaa)>,
	mut msaa: ResMut<Msaa>,
) {
	if settings.is_changed() {
		for (mut cam, mut fxaa) in &mut cam_q {
			cam.hdr = settings.hdr;
			msaa.samples = if settings.msaa { 4 } else { 1 };
			fxaa.enabled = settings.fxaa;
		}
	}
}

/// Settings that are specific to a local game client.
/// E.g. graphics settings should not be shared between machines.
#[derive(Resource, Debug, Clone, Serialize, Deserialize)]
pub struct Settings {
	pub hdr: bool,
	pub msaa: bool,
	pub fxaa: bool,
}

impl FromWorld for Settings {
	fn from_world(world: &mut World) -> Self {
		world.get_resource::<PkvStore>().map_or(
			Self {
				hdr: false,
				msaa: false,
				fxaa: false,
			},
			|store| {
				let hdr = store.get("hdr").unwrap_or(false);
				let msaa = store.get("msaa").unwrap_or(false);
				let fxaa = store.get("fxaa").unwrap_or(false);
				Self { hdr, msaa, fxaa }
			},
		)
	}
}
