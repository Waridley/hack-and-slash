use crate::planet::PlanetVec3;
use bevy::prelude::*;
use super::terrain::Ground;

#[derive(Bundle, Debug)]
pub struct ChunkBundle {
	pub center: PlanetVec3,
	pub ground: Ground,
}

