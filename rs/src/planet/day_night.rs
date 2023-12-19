use bevy::{prelude::*, render::extract_resource::ExtractResource};
#[cfg(feature = "debugging")]
use bevy_inspector_egui::prelude::*;

const SECS_PER_MIN: f64 = 60.0;

pub fn plugin(app: &mut App) -> &mut App {
	app.add_systems(Update, update_day_night)
}

#[derive(Resource, ExtractResource, Clone, Debug, Reflect)]
#[cfg_attr(feature = "debugging", derive(InspectorOptions))]
#[reflect(Resource)]
#[cfg_attr(feature = "debugging", reflect(InspectorOptions))]
pub struct DayNightCycle {
	pub mode: DayNightMode,
	#[cfg_attr(feature = "debugging", inspector(min = 1.0, speed = 5.0))] // min 1s, 5s increments
	day_length: f64,
	#[cfg_attr(feature = "debugging", inspector(min = 0.0, max = 1.0, speed = 0.001))]
	pub time_of_day: f64,
	#[cfg_attr(feature = "debugging", inspector(min = 0.0, max = 1.0, speed = 0.0001))]
	pub daylight: f64,
	pub sun_direction: Vec3,
	pub moon_direction: Vec3,
}

impl Default for DayNightCycle {
	fn default() -> Self {
		Self {
			mode: default(),
			day_length: 10.0 * SECS_PER_MIN,
			time_of_day: 0.0,
			daylight: 0.0,
			sun_direction: Vec3::splat(1.0).normalize(),
			moon_direction: Vec3::splat(-1.0).normalize(),
		}
	}
}

#[derive(Default, Clone, Copy, Debug, Reflect)]
pub enum DayNightMode {
	/// Running normally
	#[default]
	Running,
	/// Set all values manually
	Manual,
	/// `time_of_day` is set manually, but update other values based on it
	ManualToD,
	/// Time runs normally, but daylight does not change. For accessibility.
	OverrideDaylight,
}

pub fn update_day_night(mut day_night: ResMut<DayNightCycle>, t: Res<Time>) {
	let new_tod = || {
		let dt = t.delta_seconds_f64();
		(day_night.time_of_day + (dt / day_night.day_length)) % 1.0
	};
	let daylight = |tod: f64| 1.0 - ((tod - 0.5).abs() * 2.0);

	match day_night.mode {
		DayNightMode::Running => {
			day_night.time_of_day = new_tod();
			day_night.daylight = daylight(day_night.time_of_day);
		}
		DayNightMode::Manual => {}
		DayNightMode::ManualToD => day_night.daylight = daylight(day_night.time_of_day),
		DayNightMode::OverrideDaylight => day_night.time_of_day = new_tod(),
	}
}
