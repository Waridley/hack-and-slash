use bevy::{prelude::*, render::extract_resource::ExtractResource};
use std::f64::consts::TAU;

pub const DAY_LENGTH_MINUTES: f64 = 20.0;
const SECS_PER_MIN: f64 = 60.0;
const DAY_LENGTH_SECS: f64 = DAY_LENGTH_MINUTES * SECS_PER_MIN;

pub fn plugin(app: &mut App) -> &mut App {
	app.add_systems(Update, update_day_night)
}

#[derive(Resource, ExtractResource, Clone, Debug)]
pub struct DayNightCycle {
	pub mode: DayNightMode,
	pub time_of_day: f64,
	pub daylight: f64,
	pub sun_position: Vec3,
	pub moon_position: Vec3,
}

impl Default for DayNightCycle {
	fn default() -> Self {
		Self {
			mode: default(),
			time_of_day: 0.0,
			daylight: 0.0,
			sun_position: Vec3::splat(1.0).normalize(),
			moon_position: Vec3::splat(-1.0).normalize(),
		}
	}
}

#[derive(Default, Clone, Copy, Debug)]
pub enum DayNightMode {
	#[default]
	Running,
	Forced,
}

pub fn update_day_night(mut day_night: ResMut<DayNightCycle>, t: Res<Time>) {
	match day_night.mode {
		DayNightMode::Running => {
			let tod = (day_night.time_of_day + t.delta_seconds_f64() / DAY_LENGTH_SECS) % 1.0;
			day_night.time_of_day = tod;
			let cos_tod = -(tod * TAU).cos();
			let tod_norm = (cos_tod + 1.0) * 0.5; // Remap to 0..=1
			day_night.daylight = (((cos_tod * cos_tod) + 1.0) * 0.5) // Linger mid-day a little longer
				* tod_norm * tod_norm // Make night last longer
		}
		DayNightMode::Forced => {}
	}
}
