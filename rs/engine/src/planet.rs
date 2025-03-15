use crate::{
	planet::{
		chunks::{ChunkIndex, LoadedChunks},
		day_night::DayNightCycle,
		frame::PlanetFramePlugin,
		weather::WeatherPlugin,
	},
	util::{Diff, IntoFnPlugin},
};
use bevy::prelude::*;
use bevy_rapier3d::na::{Vector2, Vector3};
use serde::{Deserialize, Serialize};
use std::ops::{Add, Deref, DerefMut, Sub};

pub mod chunks;
pub mod day_night;
pub mod frame;
pub mod seeds;
pub mod sky;
pub mod terrain;
pub mod weather;

pub fn plugin(app: &mut App) -> &mut App {
	app.add_plugins((
		terrain::plugin.plugfn(),
		day_night::plugin.plugfn(),
		PlanetFramePlugin,
		WeatherPlugin,
	))
	.init_resource::<DayNightCycle>()
	.register_type::<DayNightCycle>()
	.init_resource::<LoadedChunks>()
}

#[derive(Default, Debug, Copy, Clone, PartialEq, Serialize, Deserialize, Reflect)]
#[repr(C)]
pub struct PlanetVec3 {
	x: f64,
	y: f64,
	z: f64,
}

impl PlanetVec3 {
	pub fn new(x: f64, y: f64, z: f64) -> Self {
		Self::from(Vector3::new(x, y, z))
	}
}

impl From<Vector3<f64>> for PlanetVec3 {
	#[inline(always)]
	fn from(value: Vector3<f64>) -> Self {
		Self {
			x: value.x,
			y: value.y,
			z: value.z,
		}
	}
}

impl From<Vec3> for PlanetVec3 {
	fn from(value: Vec3) -> Self {
		Self::from(Vector3::new(value.x as f64, value.y as f64, value.z as f64))
	}
}

impl Deref for PlanetVec3 {
	type Target = Vector3<f64>;

	fn deref(&self) -> &Self::Target {
		// SAFETY: Vector3 is `#[repr(C)]` and transmutes to `XY<T> { x: T, y: T, z: T } in `nalgebra::coordinates`
		unsafe { &*(self as *const Self as *const _) }
	}
}

impl DerefMut for PlanetVec3 {
	fn deref_mut(&mut self) -> &mut Self::Target {
		// SAFETY: Vector3 is `#[repr(C)]` and transmutes to `XY<T> { x: T, y: T, z: T } in `nalgebra::coordinates`
		unsafe { &mut *(self as *mut Self as *mut _) }
	}
}

impl Diff for PlanetVec3 {
	type Delta = Vec3;

	fn delta_from(&self, rhs: &Self) -> Self::Delta {
		(*self - *rhs).into()
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
		Self::from(*self + *rhs)
	}
}

impl Add<Vec3> for PlanetVec3 {
	type Output = Self;

	fn add(self, rhs: Vec3) -> Self::Output {
		let rhs = Vector3::new(rhs.x as f64, rhs.y as f64, rhs.z as f64);
		Self::from(*self + rhs)
	}
}

impl Sub<PlanetVec3> for PlanetVec3 {
	type Output = Self;

	fn sub(self, rhs: Self) -> Self::Output {
		Self::from(*self - *rhs)
	}
}

impl Sub<Vec3> for PlanetVec3 {
	type Output = Self;

	fn sub(self, rhs: Vec3) -> Self::Output {
		let rhs = Vector3::new(rhs.x as f64, rhs.y as f64, rhs.z as f64);
		Self::from(*self - rhs)
	}
}

#[derive(Component, Default, Debug, Copy, Clone, PartialEq, Serialize, Deserialize, Reflect)]
#[repr(C)]
pub struct PlanetVec2 {
	x: f64,
	y: f64,
}

impl Deref for PlanetVec2 {
	type Target = Vector2<f64>;

	fn deref(&self) -> &Self::Target {
		// SAFETY: Vector2 is `#[repr(C)]` and transmutes to `XY<T> { x: T, y: T } in `nalgebra::coordinates`
		unsafe { &*(self as *const Self as *const _) }
	}
}

impl DerefMut for PlanetVec2 {
	fn deref_mut(&mut self) -> &mut Self::Target {
		// SAFETY: Vector2 is `#[repr(C)]` and transmutes to `XY<T> { x: T, y: T } in `nalgebra::coordinates`
		unsafe { &mut *(self as *mut Self as *mut _) }
	}
}

impl From<Vector2<f64>> for PlanetVec2 {
	#[inline(always)]
	fn from(value: Vector2<f64>) -> Self {
		Self {
			x: value.x,
			y: value.y,
		}
	}
}

impl PlanetVec2 {
	pub fn new(x: f64, y: f64) -> Self {
		Self { x, y }
	}

	pub fn closest_chunk(self) -> ChunkIndex {
		self.into()
	}
}

impl Diff for PlanetVec2 {
	type Delta = Vec2;

	fn delta_from(&self, rhs: &Self) -> Self::Delta {
		(*self - *rhs).into()
	}
}

impl From<Vec2> for PlanetVec2 {
	fn from(value: Vec2) -> Self {
		Self {
			x: value.x as f64,
			y: value.y as f64,
		}
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
		Self::from(*self + *rhs)
	}
}

impl Add<Vec2> for PlanetVec2 {
	type Output = Self;

	fn add(self, rhs: Vec2) -> Self::Output {
		let rhs = Vector2::new(rhs.x as f64, rhs.y as f64);
		Self::from(*self + rhs)
	}
}

impl Sub<PlanetVec2> for PlanetVec2 {
	type Output = Self;

	fn sub(self, rhs: Self) -> Self::Output {
		Self::from(*self - *rhs)
	}
}

impl Sub<Vec2> for PlanetVec2 {
	type Output = Self;

	fn sub(self, rhs: Vec2) -> Self::Output {
		let rhs = Vector2::new(rhs.x as f64, rhs.y as f64);
		Self::from(*self - rhs)
	}
}
