use bevy::{math::Vec3, prelude::default};
use std::{
	cmp::Ordering,
	collections::{
		hash_map::Entry::{Occupied, Vacant},
		BinaryHeap, HashMap,
	},
	sync::Weak,
};

use crate::nav::{
	EdgeVendor, NavEdge, NavGraph, NavGraphRef, NavIdx,
	NavIdx::{Ground, Structure},
};

#[derive(Default)]
pub struct PathTracker {
	pub came_from: HashMap<NavIdx, NavIdx>,
}

impl PathTracker {
	pub fn new() -> PathTracker {
		PathTracker {
			came_from: HashMap::new(),
		}
	}

	fn set_predecessor(&mut self, node: NavIdx, previous: NavIdx) {
		self.came_from.insert(node, previous);
	}

	fn reconstruct_path_to(&self, last: NavIdx) -> Vec<NavIdx> {
		let mut path = vec![last];

		let mut current = last;
		while let Some(&previous) = self.came_from.get(&current) {
			path.push(previous);
			current = previous;
		}

		path.reverse();

		path
	}
}

#[derive(Default)]
pub struct AStar<Cost, EdgeGen>
where
	EdgeGen: EdgeVendor + Copy + 'static,
	Cost: FnMut(NavEdge) -> f32,
{
	edge_generator: EdgeGen,
	visit_next: BinaryHeap<MinScored>,
	scores: HashMap<NavIdx, f32>,
	estimate_scores: HashMap<NavIdx, f32>,
	path_tracker: PathTracker,
	edge_cost: Cost,
}

impl<Cost, EdgeGen> AStar<Cost, EdgeGen>
where
	EdgeGen: EdgeVendor + Copy + 'static,
	Cost: FnMut(NavEdge) -> f32,
{
	pub fn new(edge_generator: EdgeGen, edge_cost: Cost) -> Self {
		Self {
			edge_generator,
			visit_next: default(),
			scores: default(),
			estimate_scores: default(),
			path_tracker: default(),
			edge_cost,
		}
	}
	pub fn run(
		&mut self,
		graph: &NavGraph,
		start: NavIdx,
		goal: NavIdx,
	) -> Option<(f32, Vec<NavIdx>)>
	where
		Cost: FnMut(NavEdge) -> f32,
	{
		let Self {
			edge_generator,
			visit_next,
			scores,
			estimate_scores,
			path_tracker,
			edge_cost,
		} = self;

		let graph = NavGraphRef {
			graph,
			edge_generator: *edge_generator,
		};

		let estimate_cost = |id| match (id, goal) {
			(Ground(start), Ground(end)) => {
				let Some((start_hm, end_hm)) = graph
					.heightmaps
					.get(&start.chunk)
					.and_then(Weak::upgrade)
					.zip(graph.heightmaps.get(&end.chunk).and_then(Weak::upgrade))
				else {
					return f32::INFINITY;
				};
				let Some((start, end)) = start_hm
					.triangle_at_id(start.tri)
					.zip(end_hm.triangle_at_id(end.tri))
				else {
					return f32::INFINITY;
				};
				Vec3::from(start.center()).distance(Vec3::from(end.center()))
			}
			(Ground(_tri), Structure(_)) => todo!(),
			(Structure(_), Ground(_tri)) => todo!(),
			(Structure(_), Structure(_)) => todo!(),
		};

		path_tracker.came_from.clear();
		scores.insert(start, 0.0);
		visit_next.push(MinScored(estimate_cost(start), start));

		while let Some(MinScored(estimate_score, node)) = visit_next.pop() {
			if node == goal {
				let path = path_tracker.reconstruct_path_to(node);
				let cost = scores[&node];
				return Some((cost, path));
			}

			// This lookup can be unwrapped without fear of panic since the node was necessarily scored
			// before adding it to `visit_next`.
			let node_score = scores[&node];

			match estimate_scores.entry(node) {
				Occupied(mut entry) => {
					// If the node has already been visited with an equal or lower score than now, then
					// we do not need to re-visit it.
					if *entry.get() <= estimate_score {
						continue;
					}
					entry.insert(estimate_score);
				}
				Vacant(entry) => {
					entry.insert(estimate_score);
				}
			}

			for edge in graph.edges(node) {
				let next = edge.target();
				let next_score = node_score + edge_cost(edge);

				match scores.entry(next) {
					Occupied(mut entry) => {
						// No need to add neighbors that we have already reached through a shorter path
						// than now.
						if *entry.get() <= next_score {
							continue;
						}
						entry.insert(next_score);
					}
					Vacant(entry) => {
						entry.insert(next_score);
					}
				}

				path_tracker.set_predecessor(next, node);
				let next_estimate_score = next_score + estimate_cost(next);
				visit_next.push(MinScored(next_estimate_score, next));
			}
		}

		None
	}
}

#[derive(Copy, Clone, Debug)]
pub struct MinScored(pub f32, pub NavIdx);

impl PartialEq for MinScored {
	#[inline]
	fn eq(&self, other: &MinScored) -> bool {
		self.cmp(other) == Ordering::Equal
	}
}

impl Eq for MinScored {}

impl PartialOrd for MinScored {
	#[inline]
	fn partial_cmp(&self, other: &MinScored) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}

impl Ord for MinScored {
	#[inline]
	fn cmp(&self, other: &MinScored) -> Ordering {
		let a = &self.0;
		let b = &other.0;
		if a == b {
			Ordering::Equal
		} else if a < b {
			Ordering::Greater
		} else if a > b {
			Ordering::Less
		} else if a.ne(a) && b.ne(b) {
			// these are the NaN cases
			Ordering::Equal
		} else if a.ne(a) {
			// Order NaN less, so that it is last in the MinScore order
			Ordering::Less
		} else {
			Ordering::Greater
		}
	}
}
