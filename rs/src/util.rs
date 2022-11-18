use bevy_rapier3d::prelude::Real;
use rand::{distributions::Standard, prelude::Distribution};
use std::{
	sync::atomic::{AtomicUsize, Ordering::Relaxed},
	ops::Index,
};

pub fn quantize<const BITS: u8>(angle: Real) -> Real {
	let d = angle * ((2u32.pow(BITS as u32) + 1) as Real);
	let t = d - angle;
	d - t
}

#[derive(Debug)]
pub struct Noise<T> {
	values: Vec<T>,
	index: AtomicUsize,
}

impl<T> Clone for Noise<T>
where Vec<T>: Clone {
	fn clone(&self) -> Self {
		Self {
			values: self.values.clone(),
			index: AtomicUsize::new(rand::random())
		}
	}
}

impl<T> Noise<T>
where
	Standard: Distribution<T>,
{
	pub fn new(size: usize) -> Self {
		Self {
			values: (0..size).into_iter().map(|_| rand::random()).collect(),
			index: AtomicUsize::new(rand::random::<usize>()),
		}
	}

	pub fn get(&self, index: usize) -> Option<&T> {
		self.values.get(index)
	}
	
	pub fn next(&self) -> &T {
		&self.values[self.index.fetch_add(1, Relaxed) % self.values.len()]
	}
}

impl<T> Iterator for Noise<T>
where
	Standard: Distribution<T>,
	T: Clone,
{
	type Item = T;
	
	fn next(&mut self) -> Option<Self::Item> {
		Some(Self::next(&*self).clone())
	}
}

impl<T> Index<usize> for Noise<T>
where
	Standard: Distribution<T>,
{
	type Output = T;
	
	fn index(&self, index: usize) -> &Self::Output {
		self.get(index).unwrap()
	}
}