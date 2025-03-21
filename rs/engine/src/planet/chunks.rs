use super::terrain::Ground;
use crate::{nav::heightmap::TriId, planet::PlanetVec2, util::Diff};
use bevy::{ecs::system::SystemParam, prelude::*};
use bevy_rapier3d::{na::Vector3, parry::shape::Triangle};
use bimap::BiMap;
use std::fmt::Display;

#[cfg(not(target_arch = "wasm32"))]
pub const CHUNK_ROWS: usize = 64;
#[cfg(not(target_arch = "wasm32"))]
pub const CHUNK_COLS: usize = 64;
#[cfg(target_arch = "wasm32")]
pub const CHUNK_ROWS: usize = 32;
#[cfg(target_arch = "wasm32")]
pub const CHUNK_COLS: usize = 32;
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

#[derive(Component, Debug, Default, Copy, Clone, Deref, DerefMut)]
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
			(value.x / (CHUNK_COLS as f64 * TERRAIN_CELL_SIZE as f64)).round() as i32,
			(value.y / (CHUNK_ROWS as f64 * TERRAIN_CELL_SIZE as f64)).round() as i32,
		)
	}
}

impl Display for ChunkIndex {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		<IVec2 as Display>::fmt(&IVec2::new(self.x, self.y), f)
	}
}

#[derive(Resource, Default, Debug, Deref, DerefMut)]
pub struct LoadedChunks(BiMap<ChunkIndex, Entity>);

impl LoadedChunks {
	pub fn closest_to(&self, point: PlanetVec2) -> Option<(ChunkIndex, Entity)> {
		let maybe_loaded = ChunkIndex::from(point);
		if let Some(id) = self.get_by_left(&maybe_loaded) {
			return Some((maybe_loaded, *id));
		}
		self.iter()
			.map(|(index, id)| (*index, *id))
			.min_by_key(|(index, _)| index.manhattan_dist(maybe_loaded))
	}

	pub fn tri_at(
		&self,
		point: PlanetVec2,
		grounds: &Query<(&ChunkCenter, &Ground)>,
	) -> Option<(ChunkIndex, ChunkCenter, TriId, Triangle)> {
		let chunk = ChunkIndex::from(point);
		let (center, ground) = grounds.get(*self.get_by_left(&chunk)?).ok()?;
		let rel = point.delta_from(&**center);
		ground
			.tri_at(rel)
			.map(|(id, tri)| (chunk, *center, id, tri))
	}

	pub fn height_at(
		&self,
		point: PlanetVec2,
		grounds: &Query<(&ChunkCenter, &Ground)>,
	) -> Option<f32> {
		self.tri_and_height_at(point, grounds)
			.map(|(_, height)| height)
	}

	pub fn tri_and_height_at(
		&self,
		point: PlanetVec2,
		grounds: &Query<(&ChunkCenter, &Ground)>,
	) -> Option<(TriId, f32)> {
		let chunk = ChunkIndex::from(point);
		let (center, ground) = grounds.get(*self.get_by_left(&chunk)?).ok()?;
		let rel = point.delta_from(&**center);
		ground.tri_and_height_at(rel)
	}
}

#[derive(SystemParam)]
pub struct ChunkFinder<'w, 's> {
	pub loaded_chunks: Res<'w, LoadedChunks>,
	pub grounds: Query<'w, 's, (&'static ChunkCenter, &'static Ground)>,
}

impl ChunkFinder<'_, '_> {
	pub fn closest_to(
		&self,
		point: PlanetVec2,
	) -> Option<(Entity, ChunkIndex, ChunkCenter, Ground)> {
		self.loaded_chunks
			.closest_to(point)
			.and_then(|(index, entity)| {
				self.grounds
					.get(entity)
					.map(|(center, ground)| (entity, index, *center, ground.clone()))
					.ok()
			})
	}

	pub fn tri_at(&self, point: PlanetVec2) -> Option<(ChunkIndex, ChunkCenter, TriId, Triangle)> {
		self.loaded_chunks.tri_at(point, &self.grounds)
	}

	pub fn height_at(&self, point: PlanetVec2) -> Option<f32> {
		self.loaded_chunks.height_at(point, &self.grounds)
	}

	pub fn tri_and_height_at(&self, point: PlanetVec2) -> Option<(TriId, f32)> {
		self.loaded_chunks.tri_and_height_at(point, &self.grounds)
	}
}
