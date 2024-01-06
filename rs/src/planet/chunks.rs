use super::terrain::Ground;
use crate::planet::PlanetVec2;
use bevy::{prelude::*, utils::HashMap};

pub const CHUNK_SIZE: f32 = 128.0;
pub const CHUNK_ROWS: usize = CHUNK_SIZE as usize;
pub const CHUNK_COLS: usize = CHUNK_SIZE as usize;

#[derive(Bundle, Debug)]
pub struct ChunkBundle {
	pub center: ChunkCenter,
	pub index: ChunkIndex,
	pub ground: Ground,
}

#[derive(Component, Debug, Default, Deref, DerefMut)]
pub struct ChunkCenter(PlanetVec2);

#[derive(Component, Copy, Clone, Default, Debug, Hash, PartialEq, Eq)]
pub struct ChunkIndex {
	pub x: i32,
	pub y: i32,
}

impl ChunkIndex {
	pub fn new(x: i32, y: i32) -> Self {
		Self { x, y }
	}
}

impl From<ChunkIndex> for ChunkCenter {
	fn from(value: ChunkIndex) -> Self {
		Self(PlanetVec2::new(
			value.x as f64 * CHUNK_SIZE as f64,
			value.y as f64 * CHUNK_SIZE as f64,
		))
	}
}

impl From<PlanetVec2> for ChunkIndex {
	fn from(value: PlanetVec2) -> Self {
		Self::new(
			((value.x / CHUNK_SIZE as f64) + 0.5) as i32,
			((value.y / CHUNK_SIZE as f64) + 0.5) as i32,
		)
	}
}

#[derive(Resource, Default, Debug, Deref, DerefMut)]
pub struct LoadedChunks(HashMap<ChunkIndex, Entity>);
