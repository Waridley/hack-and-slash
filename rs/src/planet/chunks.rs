use crate::planet::PlanetPoint;
use bevy::prelude::*;

#[derive(Bundle, Debug)]
pub struct Chunk {
	pub center: PlanetPoint,
}
