use super::terrain::Ground;
use crate::{nav::heightmap::FnsThatShouldBePub, planet::PlanetVec2, util::Diff};
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

	pub fn height_at(
		&self,
		point: PlanetVec2,
		grounds: &Query<(&ChunkCenter, &Ground)>,
	) -> Option<f32> {
		let chunk = ChunkIndex::from(point);
		let (center, ground) = grounds.get(*self.get_by_left(&chunk)?).ok()?;
		let rel = point.delta_from(&**center);
		let rel = Vec2::new(rel.x, -rel.y);
		let point = Point3::new(rel.x, 0.0, rel.y);
		let (i, j) = ground.heights.cell_at_point(&point)?;
		let (left, right) = ground.heights.triangles_at(i, j);
		
		// ZIG-ZAG
		// a -- c  c
		// | L / / |
		// |  / /  |
		// | / / R |
		// b  a -- b
		//
		// NORMAL
		// a  a -- c
		// | \ \ R |
		// |  \ \  |
		// | L \ \ |
		// b -- c  b
		//
		let tri = match (left, right) {
			(Some(left), Some(right)) => {
				let status = ground.heights.cell_status(i, j);
				let (l, r) = if status.contains(HeightFieldCellStatus::ZIGZAG_SUBDIVISION) {
					debug_assert!({
						let p = left.a - right.a;
						p.x * p.x + p.y * p.y + p.z * p.z
					} <= f32::EPSILON);
					(left.a.xy(), right.b.xy())
				} else {
					debug_assert!({
						let p = left.c - right.c;
						p.x * p.x + p.y * p.y + p.z * p.z
					} <= f32::EPSILON);
					(left.b.xy(), right.c.xy())
				};
				// Checking the squared distances to the opposing points is at least as cheap as any other solution.
				let p = point.xy();
				let dl = l - p.xy();
				let dr = r - p.xy();
				let dl = dl.x * dl.x + dl.y * dl.y;
				let dr = dr.x * dr.x + dr.y * dr.y;
				if dl < dr {
					left
				} else {
					right
				}
			}
			(None, Some(right)) => right,
			(Some(left), None) => left,
			(None, None) => return None,
		};
		
		let da = TERRAIN_CELL_SIZE - (Vec2::from(tri.a.xz()) - rel).length();
		let db = TERRAIN_CELL_SIZE - (Vec2::from(tri.b.xz()) - rel).length();
		let dc = TERRAIN_CELL_SIZE - (Vec2::from(tri.c.xz()) - rel).length();
		let sum = da + db + dc;
		Some((da / sum) * tri.a.y + (db / sum) * tri.b.y + (dc / sum) * tri.c.y)
	}
}
