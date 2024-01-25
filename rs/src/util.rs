use bevy::{
	asset::{io::Reader, AssetLoader, AsyncReadExt, BoxedFuture, LoadContext},
	ecs::{
		event::Event,
		query::ReadOnlyWorldQuery,
		system::{EntityCommands, StaticSystemParam, SystemParam, SystemParamItem},
	},
	prelude::*,
	reflect::{serde::TypedReflectDeserializer, TypeRegistration, Typed},
	scene::{SceneLoaderError, SceneLoaderError::RonSpannedError},
};
use num_traits::NumCast;
use ron::Error::InvalidValueForType;
use serde::de::DeserializeSeed;
use std::{
	cmp::Ordering,
	collections::VecDeque,
	hash::Hash,
	iter::Sum,
	marker::PhantomData,
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

pub trait Lerp<Rhs, T = f32> {
	type Output;

	fn lerp(self, rhs: Rhs, t: T) -> Self::Output;
}

impl<This, Rhs, T> Lerp<Rhs, T> for This
where
	This: Clone,
	Rhs: Sub<This>,
	<Rhs as Sub<This>>::Output: Mul<T>,
	<<Rhs as Sub<This>>::Output as Mul<T>>::Output: Add<This>,
{
	type Output = <<<Rhs as Sub<This>>::Output as Mul<T>>::Output as Add<This>>::Output;
	#[inline(always)]
	fn lerp(
		self,
		rhs: Rhs,
		t: T,
	) -> <<<Rhs as Sub<Self>>::Output as Mul<T>>::Output as Add<Self>>::Output {
		((rhs - self.clone()) * t) + self
	}
}

#[derive(Resource, Component)]
pub struct History<T> {
	max_size: usize,
	values: VecDeque<T>,
}

impl<T> Default for History<T> {
	fn default() -> Self {
		Self::new(256)
	}
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
	pub fn track_resource(mut cmds: Commands, this: Option<ResMut<Self>>, curr: Res<T>)
	where
		T: Clone + Resource,
	{
		if let Some(mut this) = this {
			this.update(curr.clone())
		} else {
			cmds.insert_resource(Self::start_with(curr.clone(), 256))
		}
	}

	/// A system for tracking component value history.
	/// ```
	/// # use bevy::prelude::{Component, Update, With};
	/// # use sond_has::util::History;
	/// # #[derive(Component, Clone)]
	/// # struct Foo;
	/// # #[derive(Component)]
	/// # struct IsPlayer;
	/// # let mut app = bevy::app::App::new();
	/// app.add_systems(Update, History::<Foo>::track_components::<With<IsPlayer>>);
	/// ```
	pub fn track_components<QueryFilter: ReadOnlyWorldQuery>(
		mut cmds: Commands,
		mut q: Query<(Entity, Option<&mut Self>, &T), QueryFilter>,
	) where
		T: Clone + Component,
	{
		for (id, this, curr) in &mut q {
			if let Some(mut this) = this {
				this.update(curr.clone());
			} else {
				cmds.entity(id)
					.try_insert(Self::start_with(curr.clone(), 256));
			}
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

	pub fn get(&self, i: usize) -> Option<&T> {
		self.values.get(i)
	}

	pub fn get_mut(&mut self, i: usize) -> Option<&mut T> {
		self.values.get_mut(i)
	}

	pub fn n_ago(&self, n: usize) -> Option<&T> {
		self.get(self.values.len() - 1 - n)
	}

	pub fn n_ago_mut(&mut self, n: usize) -> Option<&mut T> {
		self.get_mut(self.values.len() - 1 - n)
	}

	pub fn len(&self) -> usize {
		self.values.len()
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

#[derive(Copy, Clone, Debug, PartialOrd, PartialEq, Ord, Eq, Hash)]
pub enum Todo {}

pub struct RonReflectAssetLoader<T> {
	pub registry: AppTypeRegistry,
	pub registration: TypeRegistration,
	pub extensions: Vec<&'static str>,
	pub _marker: PhantomData<T>,
}

impl<T: Reflect + Typed + TypePath> RonReflectAssetLoader<T> {
	pub fn new(registry: AppTypeRegistry, extensions: Vec<&'static str>) -> Self {
		Self {
			registry,
			registration: TypeRegistration::of::<T>(),
			extensions,
			_marker: default(),
		}
	}
}

impl<'this, T: Reflect + FromReflect + Asset> AssetLoader for RonReflectAssetLoader<T> {
	type Asset = T;
	type Settings = ();
	type Error = SceneLoaderError;

	fn load<'a>(
		&'a self,
		reader: &'a mut Reader,
		_settings: &'a Self::Settings,
		_load_context: &'a mut LoadContext,
	) -> BoxedFuture<'a, Result<Self::Asset, Self::Error>> {
		let registration = self.registration.clone();
		let registry = self.registry.clone();
		Box::pin(async move {
			let mut buf = Vec::new();
			reader.read_to_end(&mut buf).await?;
			let registry = registry.read();
			let seed = TypedReflectDeserializer::new(&registration, &*registry);
			let mut de = ron::Deserializer::from_bytes(&*buf)?;
			let val = seed
				.deserialize(&mut de)
				.map_err(|e| RonSpannedError(de.span_error(e)))?;
			T::take_from_reflect(val).map_err(|e| {
				RonSpannedError(de.span_error(InvalidValueForType {
					expected: T::type_path().into(),
					found: format!("{e:?}"),
				}))
			})
		})
	}

	fn extensions(&self) -> &[&str] {
		&self.extensions
	}
}

pub trait LerpSlerp<Rhs = Self, T = f32> {
	fn lerp_slerp(self, rhs: Rhs, t: T) -> Self;
}

impl LerpSlerp for Transform {
	fn lerp_slerp(self, other: Self, t: f32) -> Self {
		Transform {
			translation: self.translation.lerp(other.translation, t),
			rotation: self.rotation.slerp(other.rotation, t),
			scale: self.scale.lerp(other.scale, t),
		}
	}
}

/// Works just like `Sub` but takes parameters by reference, the right-hand side can only be Self,
/// and is specifically intended for animation blending.
pub trait Diff {
	/// Type representing the difference between two values of Self.
	type Delta;

	/// Subtract `rhs` from `self`
	fn relative_to(&self, rhs: &Self) -> Self::Delta;
}

impl<T: Sub<Output = D> + Clone, D> Diff for &T {
	type Delta = D;

	fn relative_to(&self, rhs: &Self) -> Self::Delta {
		(*self).clone() - (*rhs).clone()
	}
}

#[derive(Component, Resource, Deref, DerefMut)]
pub struct Prev<T>(T);

impl<T: Clone> Prev<T> {
	pub fn update_component(mut cmds: Commands, mut q: Query<(Entity, &T, Option<&mut Self>), Changed<T>>) where T: Component {
		for (id, curr, mut prev) in &mut q {
			if let Some(mut prev) = prev {
				**prev = curr.clone();
			} else {
				cmds.entity(id).insert(Self(curr.clone()));
			}
		}
	}
	
	pub fn update_res(mut cmds: Commands, curr: Res<T>, prev: Option<ResMut<Self>>) where T: Resource {
		if curr.is_changed() {
			if let Some(mut prev) = prev {
				**prev = curr.clone();
			} else {
				cmds.insert_resource(Self(curr.clone()));
			}
		}
	}
}