use crate::planet::chunks::CHUNK_SCALE;
use bevy::{
	prelude::{Reflect, Resource},
	render::extract_resource::ExtractResource,
};

#[derive(Resource, ExtractResource, Reflect, Clone, Debug)]
pub struct Weather {
	pub fog_start: f32,
	pub fog_end: f32,
}

impl Weather {
	pub fn new(start: f32, end: f32) -> Self {
		Self {
			fog_start: start,
			fog_end: end,
		}
	}
}

impl Default for Weather {
	fn default() -> Self {
		Self::new(CHUNK_SCALE.x, CHUNK_SCALE.x * 2.0)
	}
}
