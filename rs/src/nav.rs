use crate::{
	nav::{alg::AStar, heightmap::TriId},
	planet::chunks::ChunkIndex,
	util::Todo,
};
use bevy::{prelude::*, utils::HashMap};
use rapier3d::prelude::*;
use std::{collections::HashSet, sync::Weak};

pub mod alg;
pub mod heightmap;

/// TODO: Issue [#33](https://github.com/Waridley/hack-and-slash/issues/33)
#[derive(Resource, Debug)]
pub struct NavGraph {
	heightmaps: HashMap<ChunkIndex, Weak<HeightField>>,
	_structures: HashMap<Todo, Weak<Todo>>,
	_islands: HashMap<Todo, Weak<Todo>>,
	_heightmap_structure_seams: HashMap<Todo, Todo>, // should probably be bi-directional map
	_island_structure_seams: HashMap<Todo, Todo>,    // should probably be bi-directional map
}

/// ID of a triangle in the world by chunk index and triangle index within that chunk.
#[derive(Default, Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct PlanetTri {
	pub chunk: ChunkIndex,
	pub tri: TriId,
}

#[derive(Copy, Clone, Debug, Deref)]
pub struct NavGraphRef<'a, EdgeGen> {
	#[deref]
	graph: &'a NavGraph,
	edge_generator: EdgeGen,
}

impl<'a, EdgeGen: EdgeVendor> NavGraphRef<'a, EdgeGen> {
	pub fn new(graph: &'a NavGraph, edge_generator: EdgeGen) -> Self {
		Self {
			graph,
			edge_generator,
		}
	}
}

impl<'a, EdgeGen: EdgeVendor + Copy + 'static> NavGraphRef<'a, EdgeGen> {
	fn edges(self, a: NavIdx) -> impl Iterator<Item = NavEdge> + 'a + 'static {
		let Self {
			graph,
			edge_generator,
			..
		} = self;
		let iter = edge_generator.find_edges(graph, a);
		iter
	}

	pub fn neighbors(self, a: NavIdx) -> impl Iterator<Item = NavIdx> + 'a + 'static {
		let Self {
			graph,
			edge_generator,
		} = self;
		let iter = edge_generator.find_edges(graph, a);
		iter.map(move |edge| match edge {
			NavEdge::GroundToGround { target, .. } => NavIdx::Ground(target),
			NavEdge::StructureToStructure { target, .. } => NavIdx::Structure(target),
			NavEdge::GroundToStructure { target, .. } => NavIdx::Structure(target),
			NavEdge::StructureToGround { target, .. } => NavIdx::Ground(target),
		})
	}

	// PERF: may want custom set of FixedBitSets for different nodes, rather than huge HashSet
	pub fn visit_map(&self) -> HashSet<NavIdx> {
		HashSet::new()
	}

	pub fn reset_map(&self, map: &mut HashSet<NavIdx>) {
		map.clear()
	}
}

// impl<'a, EdgGen: Copy> IntoEdgeReferences for NavGraphRef<'a, EdgGen> {
// 	type EdgeRef = NavEdge;
// 	type EdgeReferences = EdgeRefs;
//
// 	fn edge_references(self) -> Self::EdgeReferences {
// 		EdgeRefs
// 	}
// }
//
// pub struct EdgeRefs;
//
// impl Iterator for EdgeRefs {
// 	type Item = NavEdge;
//
// 	fn next(&mut self) -> Option<Self::Item> {
// 		todo!()
// 	}
// }

impl<'a, EdgeGen: EdgeVendor + Copy + 'static> NavGraphRef<'a, EdgeGen> {
	pub async fn astar(
		self,
		start: NavIdx,
		goal: NavIdx,
		edge_cost: impl FnMut(NavEdge) -> f32,
	) -> Option<(f32, Vec<NavIdx>)> {
		AStar::new(self.edge_generator, edge_cost).run(self.graph, start, goal)
	}
}

pub struct NavMesh {
	_vertices: Vec<Vec3>,
	_triangle_indices: Vec<[usize; 3]>,
}

pub struct NavVolume {
	_shape: TypedShape<'static>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum NavIdx {
	Ground(PlanetTri),
	Structure(Todo),
}

/// A trait for functions that provide edges for `NavGraph::into_edges` without creating work for
/// the pathfinding algorithm for edges that a given entity will never be able to traverse.
///
/// This is basically a trait alias for a complicated `FnMut` bound.
pub trait EdgeVendor {
	fn find_edges(self, graph: &NavGraph, node: NavIdx) -> impl Iterator<Item = NavEdge> + 'static;
}

pub struct SumEdges<A, B> {
	pub a: A,
	pub b: B,
}

impl<A: EdgeVendor, B: EdgeVendor> EdgeVendor for SumEdges<A, B> {
	fn find_edges(self, graph: &NavGraph, node: NavIdx) -> impl Iterator<Item = NavEdge> + 'static {
		self.a
			.find_edges(graph, node)
			.chain(self.b.find_edges(graph, node))
	}
}

pub trait CombineVendors: EdgeVendor {
	fn plus<Other: EdgeVendor>(self, next: Other) -> impl EdgeVendor;
}

impl<T: EdgeVendor> CombineVendors for T {
	fn plus<Other: EdgeVendor>(self, next: Other) -> impl EdgeVendor {
		SumEdges { a: self, b: next }
	}
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum NavEdge {
	GroundToGround {
		source: PlanetTri,
		target: PlanetTri,
	},
	StructureToStructure {
		source: Todo,
		target: Todo,
	},
	GroundToStructure {
		source: PlanetTri,
		target: Todo,
	},
	StructureToGround {
		source: Todo,
		target: PlanetTri,
	},
}

impl NavEdge {
	pub fn target(self) -> NavIdx {
		match self {
			NavEdge::GroundToGround { target, .. } => NavIdx::Ground(target),
			NavEdge::StructureToStructure { target, .. } => NavIdx::Structure(target),
			NavEdge::GroundToStructure { target, .. } => NavIdx::Structure(target),
			NavEdge::StructureToGround { target, .. } => NavIdx::Ground(target),
		}
	}
}

pub trait NavEdgeFilter {
	fn test(&mut self, edge: NavEdge) -> bool;
}
