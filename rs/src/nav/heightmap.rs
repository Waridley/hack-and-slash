use super::{EdgeVendor, NavEdge, NavEdgeFilter, NavGraph, NavIdx, PlanetTri};
use crate::planet::chunks::{ChunkIndex, CHUNK_COLS, CHUNK_ROWS};
use bevy::{prelude::*, utils::HashMap};
use rapier3d::geometry::HeightField;
use std::{f32::consts::SQRT_2, sync::Weak};

pub trait FnsThatShouldBePub {
	fn num_triangles(&self) -> usize;

	fn split_triangle_id(&self, id: u32) -> (usize, usize, bool);

	fn triangle_id(&self, i: usize, j: usize, left: bool) -> u32;
}

impl FnsThatShouldBePub for HeightField {
	fn num_triangles(&self) -> usize {
		num_tris(self.nrows(), self.ncols())
	}

	fn split_triangle_id(&self, id: u32) -> (usize, usize, bool) {
		split_triangle_id(self.nrows(), self.ncols(), id)
	}

	fn triangle_id(&self, i: usize, j: usize, left: bool) -> u32 {
		tri_id(self.nrows(), self.ncols(), i, j, left)
	}
}

/// Triangle index within a specific chunk's `HeightField`.
pub type TriId = u32;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct TriPair {
	pub source: TriId,
	pub target: TriId,
}

impl TriPair {
	pub fn new(source: TriId, target: TriId) -> Self {
		Self { source, target }
	}
}

pub const fn num_tris(nrows: usize, ncols: usize) -> usize {
	nrows * ncols * 2
}

pub const fn split_triangle_id(nrows: usize, ncols: usize, id: TriId) -> (usize, usize, bool) {
	let num_tris = num_tris(nrows, ncols);
	let left = id < num_tris as u32 / 2;
	let cell_id = if left {
		id as usize
	} else {
		id as usize - (nrows * ncols)
	};
	let j = cell_id / nrows;
	let i = cell_id - j * nrows;
	(i, j, left)
}

pub const fn tri_id(nrows: usize, ncols: usize, i: usize, j: usize, left: bool) -> TriId {
	let tid = j * nrows + i;
	if left {
		tid as u32
	} else {
		(tid + num_tris(nrows, ncols) / 2) as u32
	}
}

#[derive(Debug, Copy, Clone)]
pub struct GroundEdges<F: NavEdgeFilter + Clone + 'static> {
	filter: F,
}

impl<F: NavEdgeFilter + Clone> EdgeVendor for GroundEdges<F> {
	fn find_edges<'a>(
		self,
		graph: &'a NavGraph,
		node: NavIdx,
	) -> impl Iterator<Item = NavEdge> + 'static {
		match node {
			NavIdx::Ground(id) => graph
				.heightmaps
				.get(&id.chunk)
				.and_then(Weak::upgrade)
				.map_or_else(
					|| GroundTriEdgeIter::none(self.filter.clone()),
					|heights| {
						GroundTriEdgeIter::new(
							heights.nrows(),
							heights.ncols(),
							id,
							self.filter.clone(),
						)
					},
				),
			_ => {
				bevy::log::error!("I think this should be unreachable, might want to handle this case explicitly if not");
				GroundTriEdgeIter::none(self.filter.clone())
			}
		}
	}
}

pub struct GroundTriEdgeIter<F> {
	nrows: usize,
	ncols: usize,
	source: PlanetTri,
	filter: F,
	curr: TriNeighbor,
}

impl<F> GroundTriEdgeIter<F> {
	fn new(nrows: usize, ncols: usize, source: PlanetTri, filter: F) -> Self {
		Self {
			nrows,
			ncols,
			source,
			filter,
			curr: TriNeighbor::A,
		}
	}
	fn none(filter: F) -> Self {
		Self {
			nrows: 0,
			ncols: 0,
			source: default(),
			filter,
			curr: TriNeighbor::Done,
		}
	}

	fn num_tris(&self) -> usize {
		num_tris(self.nrows, self.ncols)
	}

	fn split_tri_id(&self) -> (usize, usize, bool) {
		split_triangle_id(self.nrows, self.ncols, self.source.tri)
	}

	fn tri_id(&self, i: usize, j: usize, left: bool) -> u32 {
		let tid = j * self.nrows + i;
		if left {
			tid as u32
		} else {
			(tid + self.num_tris() / 2) as u32
		}
	}
}

impl<F: NavEdgeFilter> Iterator for GroundTriEdgeIter<F> {
	type Item = NavEdge;

	fn next(&mut self) -> Option<Self::Item> {
		use TriNeighbor::*;
		let source = self.source;
		let chunk = source.chunk;
		let (i, j, left) = self.split_tri_id();
		loop {
			match (self.curr, left) {
				(A, left) => {
					self.curr = B;

					// Same square, other triangle
					let a = NavEdge::GroundToGround {
						source,
						target: PlanetTri {
							chunk,
							tri: self.tri_id(i, j, !left),
						},
					};

					if self.filter.test(a) {
						return Some(a);
					}
				}
				(B, true) => {
					self.curr = C;

					let up = if i > 0 {
						NavEdge::GroundToGround {
							source,
							target: PlanetTri {
								chunk,
								tri: self.tri_id(i - 1, j, false),
							},
						}
					} else {
						NavEdge::GroundToGround {
							source,
							target: PlanetTri {
								chunk: ChunkIndex {
									x: chunk.x,
									y: chunk.y - 1,
								},
								tri: self.tri_id(self.nrows - 1, j, false),
							},
						}
					};

					if self.filter.test(up) {
						return Some(up);
					}
				}
				(B, false) => {
					self.curr = C;

					let right = if j < self.ncols - 1 {
						NavEdge::GroundToGround {
							source,
							target: PlanetTri {
								chunk,
								tri: self.tri_id(i, j + 1, true),
							},
						}
					} else {
						NavEdge::GroundToGround {
							source,
							target: PlanetTri {
								chunk: ChunkIndex {
									x: chunk.x + 1,
									y: chunk.y,
								},
								tri: self.tri_id(i, 0, true),
							},
						}
					};

					if self.filter.test(right) {
						return Some(right);
					}
				}
				(C, true) => {
					self.curr = Done;

					let left = if j > 0 {
						NavEdge::GroundToGround {
							source,
							target: PlanetTri {
								chunk,
								tri: self.tri_id(i, j - 1, false),
							},
						}
					} else {
						NavEdge::GroundToGround {
							source,
							target: PlanetTri {
								chunk: ChunkIndex {
									x: chunk.x - 1,
									y: chunk.y,
								},
								tri: self.tri_id(i, self.ncols - 1, false),
							},
						}
					};

					if self.filter.test(left) {
						return Some(left);
					}
				}
				(C, false) => {
					self.curr = Done;

					let down = if i < self.nrows - 1 {
						NavEdge::GroundToGround {
							source,
							target: PlanetTri {
								chunk,
								tri: self.tri_id(i + 1, j, true),
							},
						}
					} else {
						NavEdge::GroundToGround {
							source,
							target: PlanetTri {
								chunk: ChunkIndex {
									x: chunk.x + 1,
									y: chunk.y,
								},
								tri: self.tri_id(i, 0, true),
							},
						}
					};

					if self.filter.test(down) {
						return Some(down);
					}
				}
				(Done, _) => return None,
			}
		}
	}
}

pub struct AllGroundTris;

impl NavEdgeFilter for AllGroundTris {
	fn test(&mut self, edge: NavEdge) -> bool {
		matches!(edge, NavEdge::GroundToGround { .. })
	}
}

pub struct MaxClimb<'heights> {
	pub chunks: &'heights HashMap<ChunkIndex, Weak<HeightField>>,
	pub radians: f32,
}

impl NavEdgeFilter for MaxClimb<'_> {
	fn test(&mut self, edge: NavEdge) -> bool {
		let NavEdge::GroundToGround { source, target } = edge else {
			return false;
		};

		let (si, sj, sl) = split_triangle_id(CHUNK_ROWS, CHUNK_COLS, source.tri);
		let (ti, tj, _tl) = split_triangle_id(CHUNK_ROWS, CHUNK_COLS, target.tri);
		let (si, sj, ti, tj) = (si as f32, sj as f32, ti as f32, tj as f32);
		let dir = if si == ti && sj == tj {
			// same cell, towards other triangle
			if sl {
				Vec3::new(SQRT_2, 0.0, SQRT_2)
			} else {
				Vec3::new(-SQRT_2, 0.0, -SQRT_2)
			}
		} else {
			// towards closest triangle in adjacent cell
			Vec3::new(tj - sj, 0.0, ti - si).normalize()
		};
		let Some(snorm) = self
			.chunks
			.get(&source.chunk)
			.and_then(Weak::upgrade)
			.and_then(|heights| heights.triangle_at_id(source.tri))
			.and_then(|tri| tri.normal())
			.as_deref()
			.copied()
		else {
			return false;
		};
		if snorm.angle(&Vec3::Y.into()) > self.radians {
			// Already on a steep slope
			let dot = dir.dot(snorm.into());
			if dot < 0.0 {
				// Against normal -- Uphill
				false
			} else {
				// With normal -- Downhill
				true
			}
		} else {
			// Not on steep slope, check slope of target
			let Some(tnorm) = self
				.chunks
				.get(&target.chunk)
				.and_then(Weak::upgrade)
				.and_then(|heights| heights.triangle_at_id(source.tri))
				.and_then(|tri| tri.normal())
				.as_deref()
				.copied()
			else {
				return false;
			};
			let dot = dir.dot(tnorm.into());
			if dot < 0.0 {
				// Against normal, so uphill
				tnorm.angle(&Vec3::Y.into()) <= self.radians
			} else {
				// Normal is pointing away from us, so downhill
				true
			}
		}
	}
}

#[derive(Default, Copy, Clone, Debug)]
enum TriNeighbor {
	#[default]
	A,
	B,
	C,
	Done,
}
