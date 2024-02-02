use super::terrain::Ground;
use crate::planet::PlanetVec2;
use bevy::prelude::*;
use bevy_rapier3d::na::Vector3;
use bimap::BiMap;

pub const CHUNK_ROWS: usize = 128;
pub const CHUNK_COLS: usize = 128;
pub const TERRAIN_CELL_SIZE: f32 = 16.0;
pub const CHUNK_SCALE: Vector3<f32> = Vector3::new(
	CHUNK_COLS as f32 * TERRAIN_CELL_SIZE,
	1024.0,
	CHUNK_ROWS as f32 * TERRAIN_CELL_SIZE,
);

#[derive(Bundle, Debug)]
pub struct ChunkBundle {
	pub center: ChunkCenter,
	pub index: ChunkIndex,
	pub ground: Ground,
}

#[derive(Component, Debug, Default, Deref, DerefMut)]
pub struct ChunkCenter(pub PlanetVec2);

#[derive(Component, Copy, Clone, Default, Debug, Hash, PartialEq, Eq)]
pub struct ChunkIndex {
	pub x: i32,
	pub y: i32,
}

impl ChunkIndex {
	pub fn new(x: i32, y: i32) -> Self {
		Self { x, y }
	}
	pub fn center(self) -> ChunkCenter {
		self.into()
	}
	pub fn manhattan_dist(self, other: Self) -> u32 {
		(other.x - self.x).unsigned_abs() + (other.y - self.y).unsigned_abs()
	}
}

impl From<ChunkIndex> for ChunkCenter {
	fn from(value: ChunkIndex) -> Self {
		Self(PlanetVec2::new(
			value.x as f64 * (CHUNK_COLS as f64 * TERRAIN_CELL_SIZE as f64),
			value.y as f64 * (CHUNK_ROWS as f64 * TERRAIN_CELL_SIZE as f64),
		))
	}
}

impl From<PlanetVec2> for ChunkIndex {
	fn from(value: PlanetVec2) -> Self {
		Self::new(
			((value.x / (CHUNK_COLS as f64 * TERRAIN_CELL_SIZE as f64)) + 0.5) as i32,
			((value.y / (CHUNK_ROWS as f64 * TERRAIN_CELL_SIZE as f64)) + 0.5) as i32,
		)
	}
}

#[derive(Resource, Default, Debug, Deref, DerefMut)]
pub struct LoadedChunks(BiMap<ChunkIndex, Entity>);

impl LoadedChunks {
	pub fn closest_to(&self, point: PlanetVec2) -> Option<(ChunkIndex, Entity)> {
		let maybe_loaded = ChunkIndex::from(point);
		if let Some(id) = self.get_by_left(&maybe_loaded) {
			return Some((maybe_loaded, *id))
		}
		self.iter()
			.map(|(index, id)| (*index, *id))
			.min_by_key(|(index, id)| index.manhattan_dist(maybe_loaded))
	}
}