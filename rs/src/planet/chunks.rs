use super::terrain::Ground;
use crate::planet::PlanetVec3;
use bevy::prelude::*;

#[derive(Bundle, Debug)]
pub struct ChunkBundle {
	pub center: PlanetVec3,
	pub ground: Ground,
}
