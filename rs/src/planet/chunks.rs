use bevy::ecs::system::SystemParam;
use super::terrain::Ground;
use crate::{
	nav::heightmap::{FnsThatShouldBePub, TriId},
	planet::PlanetVec2,
	util::Diff,
};
use bevy::prelude::*;
use bevy_rapier3d::{
	na::{Point3, Vector3},
	parry::{query::PointQuery, shape::Triangle},
};
use bimap::BiMap;
use rapier3d::prelude::HeightFieldCellStatus;

pub const CHUNK_ROWS: usize = 64;
pub const CHUNK_COLS: usize = 64;
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
			(value.x / (CHUNK_COLS as f64 * TERRAIN_CELL_SIZE as f64)).round() as i32,
			(value.y / (CHUNK_ROWS as f64 * TERRAIN_CELL_SIZE as f64)).round() as i32,
		)
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
			.min_by_key(|(index, id)| index.manhattan_dist(maybe_loaded))
	}

	pub fn tri_at(
		&self,
		point: PlanetVec2,
		grounds: &Query<(&ChunkCenter, &Ground)>,
	) -> Option<(TriId, Triangle)> {
		let chunk = ChunkIndex::from(point);
		let (center, ground) = grounds.get(*self.get_by_left(&chunk)?).ok()?;
		let rel = point.delta_from(&**center);
		ground.tri_at(rel)
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
	pub fn closest_to(&self, point: PlanetVec2) -> Option<(ChunkIndex, Entity)> {
		self.loaded_chunks.closest_to(point)
	}
	
	pub fn tri_at(
		&self,
		point: PlanetVec2,
	) -> Option<(TriId, Triangle)> {
		self.loaded_chunks.tri_at(point, &self.grounds)
	}
	
	pub fn height_at(
		&self,
		point: PlanetVec2,
	) -> Option<f32> {
		self.loaded_chunks.height_at(point, &self.grounds)
	}
	
	pub fn tri_and_height_at(
		&self,
		point: PlanetVec2,
	) -> Option<(TriId, f32)> {
		self.loaded_chunks.tri_and_height_at(point, &self.grounds)
	}
}
