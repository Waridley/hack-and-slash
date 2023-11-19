use bevy::math::Vec3;
use bevy::prelude::{Component, Deref, DerefMut};
use bevy_rapier3d::parry::math::Vector;
use std::ops::{Add, Sub};

pub mod chunks;

#[derive(Component, Default, Debug, Copy, Clone, Deref, DerefMut, PartialEq)]
pub struct PlanetPoint(pub Vector<f64>);

impl PlanetPoint {
	pub fn relative_to(self, other: Self) -> Vec3 {
		let diff = self - other;
		diff.into()
	}
}

impl From<Vec3> for PlanetPoint {
	fn from(value: Vec3) -> Self {
		Self(Vector::new(value.x as f64, value.y as f64, value.z as f64))
	}
}

impl From<PlanetPoint> for Vec3 {
	fn from(value: PlanetPoint) -> Self {
		Vec3 {
			x: value.x as f32,
			y: value.y as f32,
			z: value.z as f32,
		}
	}
}

impl Add<PlanetPoint> for PlanetPoint {
	type Output = Self;

	fn add(self, rhs: Self) -> Self::Output {
		Self(*self + *rhs)
	}
}

impl Add<Vec3> for PlanetPoint {
	type Output = Self;

	fn add(self, rhs: Vec3) -> Self::Output {
		let rhs = Vector::new(rhs.x as f64, rhs.y as f64, rhs.z as f64);
		Self(*self + rhs)
	}
}

impl Sub<PlanetPoint> for PlanetPoint {
	type Output = Self;

	fn sub(self, rhs: Self) -> Self::Output {
		Self(*self - *rhs)
	}
}

impl Sub<Vec3> for PlanetPoint {
	type Output = Self;

	fn sub(self, rhs: Vec3) -> Self::Output {
		let rhs = Vector::new(rhs.x as f64, rhs.y as f64, rhs.z as f64);
		Self(*self - rhs)
	}
}
