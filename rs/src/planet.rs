use crate::{planet::day_night::DayNightCycle, util::IntoFnPlugin};
use bevy::prelude::*;
use bevy_rapier3d::{na::Vector2, parry::math::Vector};
use serde::{Deserialize, Serialize};
use std::ops::{Add, Sub};

pub mod chunks;
pub mod day_night;
pub mod sky;
pub mod terrain;

pub fn plugin(app: &mut App) -> &mut App {
	app.add_plugins((terrain::plugin.plugfn(), day_night::plugin.plugfn()))
		.init_resource::<DayNightCycle>()
		.register_type::<DayNightCycle>()
}

#[derive(
	Component, Default, Debug, Copy, Clone, Deref, DerefMut, PartialEq, Serialize, Deserialize,
)]
pub struct PlanetVec3(pub Vector<f64>);

impl PlanetVec3 {
	pub fn new(x: f64, y: f64, z: f64) -> Self {
		Self(Vector::new(x, y, z))
	}

	pub fn relative_to(self, other: Self) -> Vec3 {
		let diff = self - other;
		diff.into()
	}
}

impl From<Vec3> for PlanetVec3 {
	fn from(value: Vec3) -> Self {
		Self(Vector::new(value.x as f64, value.y as f64, value.z as f64))
	}
}

impl From<PlanetVec3> for Vec3 {
	fn from(value: PlanetVec3) -> Self {
		Vec3 {
			x: value.x as f32,
			y: value.y as f32,
			z: value.z as f32,
		}
	}
}

impl Add<PlanetVec3> for PlanetVec3 {
	type Output = Self;

	fn add(self, rhs: Self) -> Self::Output {
		Self(*self + *rhs)
	}
}

impl Add<Vec3> for PlanetVec3 {
	type Output = Self;

	fn add(self, rhs: Vec3) -> Self::Output {
		let rhs = Vector::new(rhs.x as f64, rhs.y as f64, rhs.z as f64);
		Self(*self + rhs)
	}
}

impl Sub<PlanetVec3> for PlanetVec3 {
	type Output = Self;

	fn sub(self, rhs: Self) -> Self::Output {
		Self(*self - *rhs)
	}
}

impl Sub<Vec3> for PlanetVec3 {
	type Output = Self;

	fn sub(self, rhs: Vec3) -> Self::Output {
		let rhs = Vector::new(rhs.x as f64, rhs.y as f64, rhs.z as f64);
		Self(*self - rhs)
	}
}

#[derive(
	Component, Default, Debug, Copy, Clone, Deref, DerefMut, PartialEq, Serialize, Deserialize,
)]
pub struct PlanetVec2(pub Vector2<f64>);

impl PlanetVec2 {
	pub fn new(x: f64, y: f64) -> Self {
		Self(Vector2::new(x, y))
	}

	pub fn relative_to(self, other: Self) -> Vec2 {
		let diff = self - other;
		diff.into()
	}
}

impl From<Vec2> for PlanetVec2 {
	fn from(value: Vec2) -> Self {
		Self(Vector2::new(value.x as f64, value.y as f64))
	}
}

impl From<PlanetVec2> for Vec2 {
	fn from(value: PlanetVec2) -> Self {
		Vec2 {
			x: value.x as f32,
			y: value.y as f32,
		}
	}
}

impl Add<PlanetVec2> for PlanetVec2 {
	type Output = Self;

	fn add(self, rhs: Self) -> Self::Output {
		Self(*self + *rhs)
	}
}

impl Add<Vec2> for PlanetVec2 {
	type Output = Self;

	fn add(self, rhs: Vec2) -> Self::Output {
		let rhs = Vector2::new(rhs.x as f64, rhs.y as f64);
		Self(*self + rhs)
	}
}

impl Sub<PlanetVec2> for PlanetVec2 {
	type Output = Self;

	fn sub(self, rhs: Self) -> Self::Output {
		Self(*self - *rhs)
	}
}

impl Sub<Vec2> for PlanetVec2 {
	type Output = Self;

	fn sub(self, rhs: Vec2) -> Self::Output {
		let rhs = Vector2::new(rhs.x as f64, rhs.y as f64);
		Self(*self - rhs)
	}
}
