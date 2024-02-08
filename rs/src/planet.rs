use crate::{
	planet::{
		chunks::{ChunkIndex, LoadedChunks},
		day_night::DayNightCycle,
		frame::PlanetFramePlugin,
	},
	util::{Diff, IntoFnPlugin},
};
use bevy::prelude::*;
use bevy_rapier3d::na::{Vector2, Vector3};
use serde::{Deserialize, Serialize};
use std::ops::{Add, Deref, DerefMut, Sub};
use crate::planet::weather::WeatherPlugin;

pub mod chunks;
pub mod day_night;
pub mod frame;
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

pub mod seeds {
	use base64::{engine::general_purpose::URL_SAFE, prelude::*, DecodeSliceError};
	use bevy::{prelude::Resource, utils::RandomState};
	use rand::{distributions::Standard, prelude::Distribution, Rng};
	use std::{
		borrow::Cow,
		fmt::{Display, Formatter},
		hash::{BuildHasher, Hasher},
	};

	#[derive(Resource, Debug, Clone)]
	pub struct PlanetSeed {
		string: Cow<'static, str>,
		hash: [u8; 24],
	}

	impl PartialEq for PlanetSeed {
		fn eq(&self, other: &Self) -> bool {
			self.hash == other.hash
		}
	}

	impl Distribution<PlanetSeed> for Standard {
		fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> PlanetSeed {
			let hash = rng.gen::<[u8; 24]>();
			let string = Cow::from(URL_SAFE.encode(hash));

			PlanetSeed { string, hash }
		}
	}

	impl<S: Into<Cow<'static, str>>> From<S> for PlanetSeed {
		fn from(value: S) -> Self {
			let string = value.into();
			let bytes = string.as_bytes();
			let bytes = if bytes.len() > Self::MAX_INPUT_LEN {
				bytes.split_at(Self::MAX_INPUT_LEN).0
			} else {
				bytes
			};

			let mut hash = [0u8; 24];

			// Randomly generated during development.
			// WARNING: Changing these will break compatibility with older seeds.
			let mut hasher = RandomState::with_seeds(
				8171301572015878809,
				11868435174398743204,
				12156415504192146545,
				13548443979666687785,
			)
			.build_hasher();
			hasher.write(bytes);
			hash[0..8].copy_from_slice(&hasher.finish().to_le_bytes());

			hasher.write(bytes);
			hash[8..16].copy_from_slice(&hasher.finish().to_le_bytes());

			hasher.write(bytes);
			hash[16..24].copy_from_slice(&hasher.finish().to_le_bytes());

			Self { string, hash }
		}
	}

	impl Display for PlanetSeed {
		fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
			self.string.fmt(f)
		}
	}

	impl PlanetSeed {
		/// Maximum number of characters that will be hashed from the input string.
		// WARNING: Changing this will break compatibility with some older seeds.
		pub const MAX_INPUT_LEN: usize = 128;

		pub fn string(&self) -> &Cow<'static, str> {
			&self.string
		}

		/// Transforms self into its canonical representation, with the string being the base64
		/// encoding of its hash.
		pub fn canonical(mut self) -> Self {
			self.make_canonical();
			self
		}

		/// Replaces the string representation of self with the canonical base64 encoding of its hash.
		pub fn make_canonical(&mut self) {
			let Self { string, hash } = self;
			let mut canonical = String::with_capacity(32);
			URL_SAFE.encode_string(hash, &mut canonical);
			debug_assert_eq!(canonical.len(), 32);
			if *string != canonical {
				*string.to_mut() = canonical;
			};
		}

		pub fn hash(&self) -> &[u8; 24] {
			&self.hash
		}

		pub fn from_canonical(
			canonical: impl Into<Cow<'static, str>>,
		) -> Result<Self, DecodeSliceError> {
			let string = canonical.into();
			assert_eq!(string.len(), 32);
			let mut hash = [0; 24];
			URL_SAFE.decode_slice(&*string, &mut hash)?;
			Ok(Self { string, hash })
		}
	}

	#[cfg(test)]
	mod tests {
		use super::PlanetSeed;

		#[test]
		fn canonical_equivalence() {
			let seed = PlanetSeed::from("This is a test seed for canonical equivalence.");
			let (seed, canonical) = (seed.clone(), seed.canonical());
			assert_ne!(seed.string, canonical.string);
			assert_eq!(seed, canonical);
		}

		#[test]
		fn version_equivalence() {
			const S: &str = "This is a test seed for seed consistency across game versions";
			const CANON: &str = "EMVUkEmSHysISOFRcpRyoTWjBDYE3IgV";
			let seed = PlanetSeed::from(S);
			let canonical = PlanetSeed::from_canonical(CANON).unwrap();
			assert_eq!(seed, canonical);
		}
	}
}
