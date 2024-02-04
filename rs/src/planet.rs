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
use std::{
	any::Any,
	ops::{Add, Deref, DerefMut, Sub},
};

pub mod chunks;
pub mod day_night;
pub mod frame;
pub mod sky;
pub mod terrain;

pub fn plugin(app: &mut App) -> &mut App {
	app.add_plugins((
		terrain::plugin.plugfn(),
		day_night::plugin.plugfn(),
		PlanetFramePlugin,
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
	use crate::planet::terrain;
	use base64::{engine::general_purpose::URL_SAFE, prelude::*, DecodeError, DecodeSliceError};
	use bevy::{
		log::{error, trace, warn},
		prelude::Resource,
	};
	use rand::random;
	use std::fmt::{Display, Formatter};

	#[derive(Resource, Debug, Copy, Clone, Default, PartialEq, Eq, Hash)]
	#[repr(C)]
	pub struct PlanetSeed {
		pub tera: TerrainSeeds,
	}

	impl PlanetSeed {
		pub const BASE64_CHARS: usize = TerrainSeeds::BASE64_CHARS;

		pub fn base64(self) -> [u8; Self::BASE64_CHARS] {
			self.tera.base64()
		}

		pub fn from_base64(bytes: impl AsRef<[u8]>) -> Result<Self, DecodeError> {
			TerrainSeeds::from_base64(bytes.as_ref()).map(|(tera, rest)| {
				if rest.len() > 0 {
					warn!(
						"Undecoded base64 characters: {}",
						String::from_utf8_lossy(rest)
					);
				}
				Self { tera }
			})
		}
	}

	impl Display for PlanetSeed {
		fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
			f.write_str(std::str::from_utf8(&self.base64()).unwrap())
		}
	}

	#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
	#[repr(C)]
	pub struct TerrainSeeds {
		pub base: u32,
		perlin: HSSeed,
		worley: HSSeed,
		billow: HSSeed,
		ridged: HSSeed,
	}

	const fn base64_chars(bytes: usize) -> usize {
		bytes.div_ceil(3) * 4
	}
	const _ASSERT4: [u8; 8] = [0; base64_chars(4)];
	const _ASSERT7: [u8; 12] = [0; base64_chars(7)];

	impl TerrainSeeds {
		/// Number of noise sources.
		pub const LEN: u8 = 4;

		/// Number of base64 characters to encode [Self::LEN].
		pub const LEN_CHARS: usize = 4;
		/// Number of base64 characters to encode [Self::base].
		pub const BASE_CHARS: usize = base64_chars(4);
		/// Number of base64 characters to encode all source seeds.
		pub const SOURCES_CHARS: usize =
			base64_chars(std::mem::size_of::<HSSeed>() * Self::LEN as usize);
		/// Total number of base64 characters to encode `Self`.
		pub const BASE64_CHARS: usize = Self::LEN_CHARS + Self::BASE_CHARS + Self::SOURCES_CHARS;

		pub fn base64(&self) -> [u8; Self::BASE64_CHARS] {
			let mut ret = [0u8; Self::BASE64_CHARS];

			{
				let (len, ret) = ret.split_at_mut(Self::LEN_CHARS);
				URL_SAFE.encode_slice(&[Self::LEN], len).unwrap();
				let (base, ret) = ret.split_at_mut(Self::BASE_CHARS);
				URL_SAFE
					.encode_slice(self.base.to_le_bytes(), base)
					.unwrap();
				let sources_bytes = [
					self.perlin.bytes(),
					self.worley.bytes(),
					self.billow.bytes(),
					self.ridged.bytes(),
				];
				let sources_bytes = unsafe {
					std::mem::transmute::<
						_,
						&[u8; Self::LEN as usize * std::mem::size_of::<HSSeed>()],
					>(&sources_bytes)
				};
				let n = URL_SAFE.encode_slice(sources_bytes, ret).unwrap();
				debug_assert_eq!(n, ret.len());
			}

			ret
		}

		pub fn from_base64(bytes: &[u8]) -> Result<(Self, &[u8]), DecodeError> {
			let (len, rest) = bytes.split_at(Self::LEN_CHARS);
			let mut buf = [u8::MAX; 3];
			let n = URL_SAFE.decode_slice(len, &mut buf).map_err(|e| match e {
				DecodeSliceError::DecodeError(e) => e,
				DecodeSliceError::OutputSliceTooSmall => unreachable!(),
			})?;
			debug_assert_eq!(n, 1);
			let len = buf[0];
			if len != Self::LEN {
				warn!("Seed is from a different version. Making a best-effort conversion.");
			}

			let (base, rest) = rest.split_at(Self::BASE_CHARS);
			let mut buf = [0u8; 4];
			let n = URL_SAFE.decode_slice_unchecked(base, &mut buf)?;
			debug_assert_eq!(n, 4);
			let base = u32::from_le_bytes(buf);

			let sources_chars = usize::min(Self::SOURCES_CHARS, rest.len());
			let (sources, rest) = rest.split_at(sources_chars);
			const SOURCES_BYTES: usize = std::mem::size_of::<HSSeed>() * TerrainSeeds::LEN as usize;
			let mut buf = [u8::MAX; SOURCES_BYTES + 2];
			match URL_SAFE.decode_slice(sources, &mut buf) {
				Ok(n) => debug_assert_eq!(n, SOURCES_BYTES),
				Err(DecodeSliceError::OutputSliceTooSmall) => unreachable!(),
				Err(DecodeSliceError::DecodeError(e)) => return Err(e),
			}
			let buf = unsafe {
				&*(&buf as *const _
					as *const [[u8; std::mem::size_of::<HSSeed>()]; Self::LEN as usize])
			};

			Ok((
				Self {
					base,
					perlin: HSSeed::from_bytes(buf[0]),
					worley: HSSeed::from_bytes(buf[1]),
					billow: HSSeed::from_bytes(buf[2]),
					ridged: HSSeed::from_bytes(buf[3]),
				},
				rest,
			))
		}

		pub fn perlin(&self) -> HSSeed {
			self.perlin
		}
		pub fn worley(&self) -> HSSeed {
			self.worley
		}
		pub fn billow(&self) -> HSSeed {
			self.billow
		}
		pub fn ridged(&self) -> HSSeed {
			self.ridged
		}
	}

	impl Default for TerrainSeeds {
		fn default() -> Self {
			let [perlin, worley, billow, ridged] = HSSeed::many_best_effort();
			Self {
				base: random(),
				perlin,
				worley,
				billow,
				ridged,
			}
		}
	}

	/// Combined heights and strength seed for [ChooseAndSmooth](terrain::noise::ChooseAndSmooth)
	#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
	#[repr(C)]
	pub struct HSSeed {
		heights: u32,
		strength: u32,
	}

	impl HSSeed {
		/// Changing the size of `HSSeed` will break compatibility with older seeds.
		const _ASSERT_SIZE_8: [u8; 8] = [0; std::mem::size_of::<Self>()];

		pub fn try_not_clashing_with(others: &[u32], attempts: usize) -> Option<Self> {
			let mut s = random();
			for _ in 0..attempts {
				if !others.contains(&s) {
					return Some(Self {
						heights: random(),
						strength: s,
					});
				} else {
					trace!("{} clashes with [{}]", URL_SAFE.encode(s.to_le_bytes()), {
						let mut encoded = String::with_capacity((others.len() * 4).div_ceil(3));
						for other in others {
							URL_SAFE.encode_string(other.to_le_bytes(), &mut encoded);
						}
						encoded
					});
					s = random();
				}
			}
			None
		}

		pub fn best_effort(others: &[u32]) -> Self {
			const ATTEMPTS: usize = 1024;
			Self::try_not_clashing_with(others, ATTEMPTS).unwrap_or_else(|| {
				let s = random();
				if others.contains(&s) {
					let mut encoded = String::with_capacity((others.len() * 4).div_ceil(3));
					for other in others {
						URL_SAFE.encode_string(other.to_le_bytes(), &mut encoded);
					}
					error!(
						"Couldn't find a non-clashing strength seed in {ATTEMPTS} attempts. \
					Using {} despite it clashing with [{encoded}].",
						URL_SAFE.encode(s.to_le_bytes())
					);
				} else {
					trace!(
						"One last attempt found {}",
						URL_SAFE.encode(s.to_le_bytes())
					);
				}
				Self {
					heights: random(),
					strength: s,
				}
			})
		}

		pub fn many_best_effort<const LEN: usize>() -> [Self; LEN] {
			let mut ret = [Self {
				heights: 0,
				strength: 0,
			}; LEN];
			let mut buf = [0; LEN];
			for i in 0..LEN {
				let new = Self::best_effort(&buf[0..i]);
				ret[i] = new;
				buf[i] = new.strength;
			}
			ret
		}

		pub fn bytes(self) -> [u8; std::mem::size_of::<Self>()] {
			unsafe {
				// All std ways of doing this are either unstable or work with slices instead of arrays
				std::mem::transmute([self.heights.to_le_bytes(), self.strength.to_le_bytes()])
			}
		}

		pub fn from_bytes(bytes: [u8; std::mem::size_of::<Self>()]) -> Self {
			let [heights, strength] = unsafe {
				// All std ways of doing this are either unstable or work with slices instead of arrays
				std::mem::transmute(bytes)
			};
			Self {
				heights: u32::from_le_bytes(heights),
				strength: u32::from_le_bytes(strength),
			}
		}

		pub fn heights(&self) -> u32 {
			self.heights
		}
		pub fn strength(&self) -> u32 {
			self.strength
		}
	}

	#[cfg(test)]
	mod tests {
		use crate::planet::seeds::PlanetSeed;

		#[test]
		fn seed_base64_round_trip() {
			let seed = PlanetSeed::default();
			let encoded = seed.base64();
			let decoded = PlanetSeed::from_base64(encoded);
			assert_eq!(Ok(seed), decoded);
		}
	}
}
