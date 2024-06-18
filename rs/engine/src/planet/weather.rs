use crate::{
	planet::chunks::{CHUNK_SCALE, TERRAIN_CELL_SIZE},
	ui::{a11y::AKNode, widgets::WidgetShape},
};
use bevy::{
	prelude::{Reflect, Resource, *},
	render::{
		extract_resource::ExtractResource, primitives::Aabb,
		view::VisibilitySystems::VisibilityPropagate,
	},
};

pub struct WeatherPlugin;

impl Plugin for WeatherPlugin {
	fn build(&self, app: &mut App) {
		app.add_systems(PostUpdate, cull_fully_fogged.before(VisibilityPropagate));
	}
}

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

pub fn cull_fully_fogged(
	cams: Query<&GlobalTransform, With<Camera>>,
	mut visibilities: Query<(&mut Visibility, &GlobalTransform, &Aabb), Without<AKNode>>,
	weather: Res<Weather>,
) {
	// Can't use `ViewVisibility` because it only allows setting visibility to `true`, not culling.

	let dist = weather.fog_end;
	for (mut vis, global, aabb) in &mut visibilities {
		// Get the closest possible point
		let radius = aabb.half_extents.length();
		let global = global.translation().xy();
		if *vis == Visibility::Hidden
			&& cams.iter().any(|cam| {
				(global - cam.translation().xy()).length() - radius < dist + TERRAIN_CELL_SIZE
			}) {
			*vis = Visibility::Inherited;
		} else if *vis == Visibility::Inherited
			&& !cams.iter().any(|cam| {
				(global - cam.translation().xy()).length() - radius < dist + TERRAIN_CELL_SIZE * 4.0
			}) {
			*vis = Visibility::Hidden;
		}
	}
}
