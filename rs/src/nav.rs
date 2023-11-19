use bevy::prelude::*;
use fixedbitset::FixedBitSet;
use petgraph::visit::{
	Data, EdgeRef, GraphBase, GraphRef, IntoEdgeReferences, IntoEdges, IntoNeighbors, VisitMap,
	Visitable,
};
use rapier3d::prelude::*;
use std::f32::consts::SQRT_2;
use std::ops::Range;

pub struct NavGraph {
	chunks: Vec<NavChunk>,
	seams: Vec<Seam>,
}

pub struct NavMesh {
	vertices: Vec<Vec3>,
	triangle_indices: Vec<[usize; 3]>,
}

pub struct NavVolume {
	shape: TypedShape<'static>,
}

pub enum NavChunk {
	Mesh(NavMesh),
	Volume(NavVolume),
}

pub struct Seam {}

#[derive(Copy, Clone, Deref)]
pub struct HeightmapNavGraph<'a, F: FnMut(TriPair) -> bool + Copy> {
	#[deref]
	heights: &'a HeightField,
	filter: F,
}

impl<'a, F: FnMut(TriPair) -> bool + Copy> HeightmapNavGraph<'a, F> {
	pub fn new(heights: &'a HeightField, filter: F) -> Self {
		Self { heights, filter }
	}

	pub fn astar(
		self,
		start: <Self as GraphBase>::NodeId,
		goal: <Self as GraphBase>::NodeId,
		edge_cost: impl FnMut(TriPair) -> f32,
	) -> Option<(f32, Vec<<Self as GraphBase>::NodeId>)> {
		petgraph::algo::astar(
			self,
			start,
			|id| id == goal,
			edge_cost,
			|id| {
				// // Manhattan distance
				// let (i, j, left) = self.split_triangle_id(id);
				// let (gi, gj, gleft) = self.split_triangle_id(goal);
				// let left = !left as usize; // treat left as 0 and right as 1
				// let gleft = !gleft as usize;
				// ((gi * 2 + gleft).abs_diff(i * 2 + left)
				// 	+ (gj * 2 + gleft).abs_diff(j * 2 + left))
				// 	as f32

				// Euclidian distance
				let goal = self.triangle_at_id(goal).unwrap().center();
				let center = self.triangle_at_id(id).unwrap().center();
				Vec3::from(goal).distance(Vec3::from(center))
			},
		)
	}
}

pub fn height_field_graph_with_max_climb<'a>(
	heights: &'a HeightField,
	max_climb_radians: f32,
) -> HeightmapNavGraph<'a, impl FnMut(TriPair) -> bool + Copy + 'a> {
	HeightmapNavGraph {
		heights,
		filter: move |pair: TriPair| {
			let (si, sj, sl) = heights.split_triangle_id(pair.source());
			let (ti, tj, _tl) = heights.split_triangle_id(pair.target());
			let (si, sj, ti, tj) = (si as f32, sj as f32, ti as f32, tj as f32);
			let dir = if si == ti && sj == tj {
				// same cell, towards other triangle
				if sl {
					Vec3::new(SQRT_2, 0.0, SQRT_2)
				} else {
					Vec3::new(-SQRT_2, 0.0, -SQRT_2)
				}
			} else {
				Vec3::new(tj - sj, 0.0, ti - si).normalize()
			};
			let snorm = heights
				.triangle_at_id(pair.source())
				.unwrap()
				.normal()
				.unwrap();
			if snorm.angle(&Vec3::Y.into()) > max_climb_radians {
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
				let tnorm = heights
					.triangle_at_id(pair.target())
					.unwrap()
					.normal()
					.unwrap();
				let dot = dir.dot(tnorm.into());
				if dot < 0.0 {
					// Against normal, so uphill
					if tnorm.angle(&Vec3::Y.into()) > max_climb_radians {
						false
					} else {
						true
					}
				} else {
					// Normal is pointing away from us, so downhill
					true
				}
			}
		},
	}
}

pub trait FnsThatShouldBePub {
	fn num_triangles(&self) -> usize;

	fn split_triangle_id(&self, id: u32) -> (usize, usize, bool);

	fn triangle_id(&self, i: usize, j: usize, left: bool) -> u32;
}

impl FnsThatShouldBePub for HeightField {
	fn num_triangles(&self) -> usize {
		self.nrows() * self.ncols() * 2
	}

	fn split_triangle_id(&self, id: u32) -> (usize, usize, bool) {
		let left = id < self.num_triangles() as u32 / 2;
		let cell_id = if left {
			id as usize
		} else {
			id as usize - self.num_triangles() / 2
		};
		let j = cell_id / self.nrows();
		let i = cell_id - j * self.nrows();
		(i, j, left)
	}

	fn triangle_id(&self, i: usize, j: usize, left: bool) -> u32 {
		let tid = j * self.nrows() + i;
		if left {
			tid as u32
		} else {
			(tid + self.num_triangles() / 2) as u32
		}
	}
}

impl<F: FnMut(TriPair) -> bool + Copy> GraphBase for HeightmapNavGraph<'_, F> {
	type EdgeId = TriPair;
	type NodeId = TriId;
}

impl<F: FnMut(TriPair) -> bool + Copy> Visitable for HeightmapNavGraph<'_, F> {
	type Map = FixedBitSet;

	fn visit_map(self: &Self) -> Self::Map {
		FixedBitSet::with_capacity(self.heights.nrows() * self.heights.ncols())
	}

	fn reset_map(self: &Self, map: &mut Self::Map) {
		if map.len() != self.heights.nrows() * self.heights.nrows() {
			*map = FixedBitSet::with_capacity(self.heights.nrows() * self.heights.ncols())
		} else {
			map.clear()
		}
	}
}

impl<'a, F: FnMut(TriPair) -> bool + Copy> IntoEdgeReferences for HeightmapNavGraph<'a, F> {
	type EdgeRef = TriPair;
	type EdgeReferences = EdgeIter<'a, F>;

	fn edge_references(self) -> Self::EdgeReferences {
		EdgeIter::new(self)
	}
}

pub struct EdgeIter<'a, F: FnMut(TriPair) -> bool + Copy> {
	graph: HeightmapNavGraph<'a, F>,
	range: Range<TriId>,
	curr_edges: Option<<Vec<TriPair> as IntoIterator>::IntoIter>,
}

impl<'a, F: FnMut(TriPair) -> bool + Copy> EdgeIter<'a, F> {
	fn new(data: HeightmapNavGraph<'a, F>) -> Self {
		let range = 0..data.num_triangles() as u32;
		Self {
			graph: data,
			range,
			curr_edges: None,
		}
	}
}

impl<F: FnMut(TriPair) -> bool + Copy> Iterator for EdgeIter<'_, F> {
	type Item = TriPair;

	fn next(&mut self) -> Option<Self::Item> {
		loop {
			if let Some(mut curr) = self.curr_edges.take() {
				if let Some(pair) = curr.next() {
					return Some(pair);
				} else {
					self.curr_edges = self.range.next().map(|id| self.graph.edges(id));
				}
			} else {
				return None;
			}
		}
	}
}

impl<F: FnMut(TriPair) -> bool + Copy> Data for HeightmapNavGraph<'_, F> {
	type NodeWeight = Vec3; // normal
	type EdgeWeight = ();
}

impl<F: FnMut(TriPair) -> bool + Copy> GraphRef for HeightmapNavGraph<'_, F> {}

impl<F: FnMut(TriPair) -> bool + Copy> IntoNeighbors for HeightmapNavGraph<'_, F> {
	type Neighbors = <Vec<TriId> as IntoIterator>::IntoIter;

	fn neighbors(mut self, a: Self::NodeId) -> Self::Neighbors {
		//TODO: Create special iterator instead of allocating vecs
		let (i, j, left) = self.split_triangle_id(a);
		let mut neighbors = Vec::new();

		let mut try_push = |this: &mut Self, target| {
			if (this.filter)(TriPair::new(a, target)) {
				neighbors.push(target);
			}
		};

		if left {
			let right = self.triangle_id(i, j, false);
			(try_push)(&mut self, right);

			if i > 0 {
				let up = self.triangle_id(i - 1, j, false);
				(try_push)(&mut self, up);
			}

			if j > 0 {
				let left = self.triangle_id(i, j - 1, false);
				(try_push)(&mut self, left);
			}
		} else {
			let left = self.triangle_id(i, j, true);
			(try_push)(&mut self, left);

			if j < self.heights.ncols() - 1 {
				let right = self.triangle_id(i, j + 1, true);
				(try_push)(&mut self, right);
			}

			if i < self.heights.nrows() - 1 {
				let down = self.triangle_id(i + 1, j, true);
				(try_push)(&mut self, down);
			}
		}
		neighbors.into_iter()
	}
}

impl<F: FnMut(TriPair) -> bool + Copy> IntoEdges for HeightmapNavGraph<'_, F> {
	type Edges = <Vec<TriPair> as IntoIterator>::IntoIter;

	fn edges(mut self, a: Self::NodeId) -> Self::Edges {
		//TODO: Create special iterator instead of allocating vecs
		let mut edges = Vec::new();
		let (i, j, left) = self.split_triangle_id(a);

		let mut try_push = |this: &mut Self, target| {
			let pair = TriPair::new(a, target);
			if (this.filter)(pair) {
				edges.push(pair);
			}
		};

		if left {
			let right = self.triangle_id(i, j, false);
			(try_push)(&mut self, right);

			if i > 0 {
				let up = self.triangle_id(i - 1, j, false);
				(try_push)(&mut self, up);
			}

			if j > 0 {
				let left = self.triangle_id(i, j - 1, false);
				(try_push)(&mut self, left);
			}
		} else {
			let left = self.triangle_id(i, j, true);
			(try_push)(&mut self, left);

			if j < self.heights.ncols() - 1 {
				let right = self.triangle_id(i, j + 1, true);
				(try_push)(&mut self, right);
			}

			if i < self.heights.nrows() - 1 {
				let down = self.triangle_id(i + 1, j, true);
				(try_push)(&mut self, down);
			}
		}
		edges.into_iter()
	}
}

type TriId = u32;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct TriPair {
	source: TriId,
	target: TriId,
}

impl TriPair {
	fn new(source: TriId, target: TriId) -> Self {
		Self { source, target }
	}
}

impl EdgeRef for TriPair {
	type NodeId = TriId;
	type EdgeId = Self;
	type Weight = ();

	fn source(&self) -> Self::NodeId {
		self.source
	}

	fn target(&self) -> Self::NodeId {
		self.target
	}

	fn weight(&self) -> &Self::Weight {
		&()
	}

	fn id(&self) -> Self::EdgeId {
		*self
	}
}
