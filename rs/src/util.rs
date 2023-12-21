use bevy::{
	ecs::{
		event::Event,
		system::{EntityCommands, StaticSystemParam, SystemParam, SystemParamItem},
	},
	prelude::*,
};
use num_traits::NumCast;
use std::{
	cmp::Ordering,
	collections::VecDeque,
	iter::Sum,
	ops::{Add, Div, Index, IndexMut, Mul, Sub},
};

#[inline(always)]
pub fn quantize<const BITS: u32>(value: f32) -> f32 {
	// Veltkamp-Dekker splitting algorithm
	debug_assert!(BITS < f32::MANTISSA_DIGITS);
	let d = value * (BITS + 1) as f32;
	let t = d - value;
	d - t
}

pub struct FnPlugin<F: for<'a> Fn(&'a mut App) -> &'a mut App + Send + Sync + 'static>(F);

impl<F> Plugin for FnPlugin<F>
where
	F: for<'a> Fn(&'a mut App) -> &'a mut App + Send + Sync + 'static,
{
	fn build(&self, app: &mut App) {
		(self.0)(app);
	}
}

pub trait IntoFnPlugin:
	for<'a> Fn(&'a mut App) -> &'a mut App + Sized + Send + Sync + 'static
{
	fn plugfn(self) -> FnPlugin<Self> {
		FnPlugin(self)
	}
}

impl<F: for<'a> Fn(&'a mut App) -> &'a mut App + Send + Sync + 'static> IntoFnPlugin for F {}

pub trait Spawnable {
	type Params: SystemParam + 'static;
	type InstanceData;
	fn spawn<'w, 's, 'a>(
		cmds: &'a mut Commands<'w, 's>,
		params: &mut SystemParamItem<'w, 's, Self::Params>,
		data: Self::InstanceData,
	) -> EntityCommands<'w, 's, 'a>;
}

#[derive(SystemParam)]
pub struct Factory<'w, 's, P: Spawnable + 'static> {
	pub cmds: Commands<'w, 's>,
	pub params: StaticSystemParam<'w, 's, <P as Spawnable>::Params>,
}

impl<'w, 's, T: Spawnable> Factory<'w, 's, T> {
	pub fn spawn<'a>(&'a mut self, data: T::InstanceData) -> EntityCommands<'w, 's, 'a> {
		let Self { cmds, params } = self;
		T::spawn(cmds, params, data)
	}
}

pub fn consume_spawn_events<T: Spawnable>(
	mut factory: Factory<T>,
	mut events: ResMut<Events<T::InstanceData>>,
) where
	<T as Spawnable>::InstanceData: Event,
{
	for event in events.drain() {
		factory.spawn(event);
	}
}

pub trait Lerp<R, T>
where
	Self: Clone,
	R: Sub<Self>,
	<R as Sub<Self>>::Output: Mul<T>,
	<<R as Sub<Self>>::Output as Mul<T>>::Output: Add<Self>,
{
	#[inline(always)]
	fn lerp(
		self,
		rhs: R,
		t: T,
	) -> <<<R as Sub<Self>>::Output as Mul<T>>::Output as Add<Self>>::Output {
		((rhs - self.clone()) * t) + self
	}
}

impl<This, R, T> Lerp<R, T> for This
where
	This: Clone,
	R: Sub<This>,
	<R as Sub<This>>::Output: Mul<T>,
	<<R as Sub<This>>::Output as Mul<T>>::Output: Add<This>,
{
}

#[derive(Resource, Component)]
pub struct History<T> {
	max_size: usize,
	values: VecDeque<T>,
}

impl<T> History<T> {
	/// A system for tracking resource value history.
	/// ```
	/// # use bevy::prelude::Update;
	/// # use sond_has::util::History;
	/// # #[derive(bevy::prelude::Resource, Clone)]
	/// # struct Foo;
	/// # let mut app = bevy::app::App::new();
	/// app
 	///     .insert_resource(Foo)
 	///     .insert_resource(History::<Foo>::new(1024))
 	///     .add_systems(Update, History::<Foo>::track_resource);
	/// ```
	pub fn track_resource(mut this: ResMut<Self>, curr: Res<T>)
	where
		T: Clone + Resource,
	{
		this.update(curr.clone())
	}

	/// A system for tracking component value history.
	/// ```
	/// # use bevy::prelude::Update;
	/// # use sond_has::util::History;
	/// # #[derive(bevy::prelude::Component, Clone)]
	/// # struct Foo;
	/// # let mut app = bevy::app::App::new();
	/// app.add_systems(Update, History::<Foo>::track_components);
	/// ```
	pub fn track_components(mut q: Query<(&mut Self, &T)>)
	where
		T: Clone + Component,
	{
		for (mut this, curr) in &mut q {
			this.update(curr.clone())
		}
	}

	/// Create a new empty history.
	pub fn new(max_size: usize) -> Self {
		Self {
			max_size,
			values: VecDeque::with_capacity(max_size),
		}
	}

	/// Create a new History with an initial value.
	pub fn start_with(value: T, max_size: usize) -> Self {
		let mut ret = Self::new(max_size);
		ret.update(value);
		ret
	}

	pub fn max_size(&self) -> usize {
		self.max_size
	}

	pub fn resize(&mut self, size: usize) {
		match size.cmp(&self.max_size) {
			Ordering::Less => {
				let start = self.values.len().saturating_sub(size);
				if start > 0 {
					let mut new = VecDeque::with_capacity(size);
					new.extend(self.values.drain(start..));
					self.values = new;
				} else {
					self.values.shrink_to(size);
				}
				self.max_size = size;
			}
			Ordering::Equal => {}
			Ordering::Greater => {
				self.max_size = size;
				self.values.reserve(size - self.max_size);
			}
		}
	}

	pub fn update(&mut self, value: T) {
		while self.values.len() >= self.max_size {
			self.values.pop_front();
		}
		self.values.push_back(value);
	}

	pub fn iter(&self) -> impl ExactSizeIterator<Item = &T> {
		self.values.iter()
	}

	pub fn iter_mut(&mut self) -> impl ExactSizeIterator<Item = &mut T> {
		self.values.iter_mut()
	}

	pub fn last(&self) -> &T {
		self.values.back().unwrap()
	}

	pub fn is_empty(&self) -> bool {
		self.values.is_empty()
	}
}

impl<T> IntoIterator for History<T> {
	type Item = T;
	type IntoIter = <VecDeque<T> as IntoIterator>::IntoIter;

	fn into_iter(self) -> Self::IntoIter {
		self.values.into_iter()
	}
}

impl<T, I> Index<I> for History<T>
where
	VecDeque<T>: Index<I>,
{
	type Output = <VecDeque<T> as Index<I>>::Output;

	fn index(&self, index: I) -> &Self::Output {
		self.values.index(index)
	}
}

impl<T, I> IndexMut<I> for History<T>
where
	VecDeque<T>: IndexMut<I>,
{
	fn index_mut(&mut self, index: I) -> &mut Self::Output {
		self.values.index_mut(index)
	}
}

pub trait Average: Sized + IntoIterator
where
	<Self as IntoIterator>::IntoIter: ExactSizeIterator,
{
	fn average<Out, S, D>(self) -> Out
	where
		S: Sum<<Self as IntoIterator>::Item>,
		S: Div<D, Output = Out>,
		D: NumCast,
	{
		let iter = self.into_iter();
		let size = iter.size_hint().0;
		iter.sum::<S>() / D::from(size).unwrap()
	}
}

impl<I: IntoIterator> Average for I
where
	Self: Sized,
	<Self as IntoIterator>::IntoIter: ExactSizeIterator,
{
}
