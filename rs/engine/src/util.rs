use std::{
	cmp::Ordering,
	collections::VecDeque,
	f32::consts::{PI, TAU},
	hash::Hash,
	iter::Sum,
	marker::PhantomData,
	ops::{Add, Div, Index, IndexMut, Mul, Sub},
	time::Duration,
};

use bevy::{
	asset::{io::Reader, AssetLoader, AsyncReadExt, BoxedFuture, LoadContext},
	ecs::{
		query::{QueryEntityError, QueryFilter},
		schedule::run_enter_schedule,
		system::{EntityCommands, StaticSystemParam, SystemParam, SystemParamItem},
	},
	prelude::*,
	reflect::{serde::TypedReflectDeserializer, TypeRegistration, Typed},
	scene::{SceneLoaderError, SceneLoaderError::RonSpannedError},
};
use num_traits::NumCast;
use ron::Error::InvalidValueForType;
use serde::{de::DeserializeSeed, Deserialize, Serialize};

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
	) -> EntityCommands<'a>;
}

#[derive(SystemParam)]
pub struct Factory<'w, 's, P: Spawnable + 'static> {
	pub cmds: Commands<'w, 's>,
	pub params: StaticSystemParam<'w, 's, <P as Spawnable>::Params>,
}

impl<'w, 's, T: Spawnable> Factory<'w, 's, T> {
	pub fn spawn(&mut self, data: T::InstanceData) -> EntityCommands {
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
	/// # use sond_has_engine::util::History;
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
	/// # use sond_has_engine::util::History;
	/// # #[derive(Component, Clone)]
	/// # struct Foo;
	/// # #[derive(Component)]
	/// # struct IsPlayer;
	/// # let mut app = bevy::app::App::new();
	/// app.add_systems(Update, History::<Foo>::track_components::<With<IsPlayer>>);
	/// ```
	pub fn track_components<Filter: QueryFilter>(
		mut cmds: Commands,
		mut q: Query<(Entity, Option<&mut Self>, &T), Filter>,
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

impl<T: Reflect + FromReflect + Asset> AssetLoader for RonReflectAssetLoader<T> {
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
			let seed = TypedReflectDeserializer::new(&registration, &registry);
			let mut de = ron::Deserializer::from_bytes(&buf)?;
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

// impl LerpSlerp for GlobalTransform {
// 	fn lerp_slerp(self, rhs: Self, t: f32) -> Self {
// 		self * (Transform::IDENTITY
// 			+ (self.reparented_to(&rhs).relative_to(&Transform::IDENTITY) * t))
// 	}
// }

/// Works just like `Sub` but takes parameters by reference, the right-hand side can only be Self,
/// and is specifically intended for animation blending.
pub trait Diff {
	/// Type representing the difference between two values of Self.
	type Delta;

	/// Subtract `rhs` from `self`
	fn delta_from(&self, rhs: &Self) -> Self::Delta;
}

impl Diff for f32 {
	type Delta = f32;

	fn delta_from(&self, rhs: &Self) -> Self::Delta {
		*self - *rhs
	}
}

impl Diff for f64 {
	type Delta = f64;

	fn delta_from(&self, rhs: &Self) -> Self::Delta {
		*self - *rhs
	}
}

macro_rules! impl_diff_for_uint {
	($u:ty : $i:ty) => {
		impl Diff for $u {
			type Delta = $i;
			fn delta_from(&self, rhs: &Self) -> Self::Delta {
				<$i as TryFrom<$u>>::try_from(*self)
					.expect(concat!("value is too large to fit into ", stringify!($i)))
					.checked_sub_unsigned(*rhs)
					.expect(concat!(
						"rhs is too larget to subtract from ",
						stringify!($u)
					))
			}
		}
	};
}

impl_diff_for_uint!(u8: i8);
impl_diff_for_uint!(u16: i16);
impl_diff_for_uint!(u32: i32);
impl_diff_for_uint!(u64: i64);
impl_diff_for_uint!(u128: i128);

macro_rules! impl_diff_for_int {
	($t:ty) => {
		impl Diff for $t {
			type Delta = Self;
			fn delta_from(&self, rhs: &Self) -> Self::Delta {
				*self - *rhs
			}
		}
	};
}

impl_diff_for_int!(i8);
impl_diff_for_int!(i16);
impl_diff_for_int!(i32);
impl_diff_for_int!(i64);
impl_diff_for_int!(i128);

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct TransformDelta(Transform);

impl TransformDelta {
	pub const ZERO: Self = Self(Transform {
		translation: Vec3::ZERO,
		rotation: Quat::IDENTITY,
		scale: Vec3::ZERO,
	});
}

impl Diff for Transform {
	type Delta = TransformDelta;

	fn delta_from(&self, rhs: &Self) -> Self::Delta {
		TransformDelta(Transform {
			translation: self.translation - rhs.translation,
			rotation: (rhs.rotation.inverse() * self.rotation).normalize(),
			scale: self.scale - rhs.scale,
		})
	}
}

impl Default for TransformDelta {
	fn default() -> Self {
		Self::ZERO
	}
}

impl Add for TransformDelta {
	type Output = Self;

	fn add(self, rhs: Self) -> Self::Output {
		Self(Transform {
			translation: self.0.translation + rhs.0.translation,
			rotation: (self.0.rotation * rhs.0.rotation).normalize(),
			scale: self.0.scale + rhs.0.scale,
		})
	}
}

impl Add<TransformDelta> for Transform {
	type Output = Self;

	fn add(self, rhs: TransformDelta) -> Self::Output {
		Self {
			translation: self.translation + rhs.0.translation,
			rotation: (self.rotation * rhs.0.rotation).normalize(),
			scale: self.scale + rhs.0.scale,
		}
	}
}

#[cfg(test)]
#[test]
fn xform_delta_add() {
	let xform1 = Transform {
		translation: Vec3::new(1.0, 2.0, 3.0),
		rotation: Quat::from_rotation_arc(Vec3::Y, Vec3::new(1.0, 1.0, 1.0).normalize()),
		scale: Vec3::ONE,
	};
	let xform2 = Transform {
		translation: Vec3::new(4.0, 5.0, 6.0),
		rotation: Quat::from_rotation_arc(Vec3::Y, Vec3::new(-1.0, 1.0, 1.0).normalize()),
		scale: Vec3::splat(2.0),
	};
	let diff = xform2.delta_from(&xform1);
	let xform1_plus_diff = xform1 + diff;
	assert_eq!(xform2.translation, xform1_plus_diff.translation);
	assert_eq!(xform2.scale, xform1_plus_diff.scale);
	let rot2 = xform2.rotation;
	let rot1d = xform1_plus_diff.rotation;
	let e = f32::EPSILON;
	assert!(
		rot2.x >= rot1d.x - e && rot2.x <= rot1d.x + e,
		"\n{rot2}\n{rot1d}"
	);
	assert!(
		rot2.y >= rot1d.y - e && rot2.y <= rot1d.y + e,
		"\n{rot2}\n{rot1d}"
	);
	assert!(
		rot2.z >= rot1d.z - e && rot2.z <= rot1d.z + e,
		"\n{rot2}\n{rot1d}"
	);
	assert!(
		rot2.w >= rot1d.w - e && rot2.w <= rot1d.w + e,
		"\n{rot2}\n{rot1d}"
	);
}

impl Mul<f32> for TransformDelta {
	type Output = Self;

	fn mul(self, rhs: f32) -> Self::Output {
		let (axis, angle) = self.0.rotation.to_axis_angle();
		Self(Transform {
			translation: self.0.translation * rhs,
			rotation: Quat::from_axis_angle(axis, angle * rhs),
			scale: self.0.scale * rhs,
		})
	}
}

impl<T: Sub<Output = D> + Clone, D> Diff for &T {
	type Delta = D;

	fn delta_from(&self, rhs: &Self) -> Self::Delta {
		(*self).clone() - (*rhs).clone()
	}
}

#[derive(Default, Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct DurationDelta {
	pub nanos: i128,
}

impl DurationDelta {
	pub const ZERO: Self = Self { nanos: 0 };
}

impl Diff for Duration {
	type Delta = DurationDelta;

	fn delta_from(&self, rhs: &Self) -> Self::Delta {
		DurationDelta {
			nanos: (self.as_nanos() as i128)
				.checked_sub_unsigned(rhs.as_nanos())
				.unwrap(),
		}
	}
}

impl Mul<f32> for DurationDelta {
	type Output = Self;

	fn mul(self, rhs: f32) -> Self::Output {
		let conj = 1.0 / (rhs as f64);
		if conj.is_finite() && conj < i128::MAX as f64 {
			Self {
				nanos: self
					.nanos
					.checked_div(conj as i128)
					.expect("Overflow when multiplying DurationDelta by f32"),
			}
		} else {
			Self::ZERO
		}
	}
}

impl Add<DurationDelta> for Duration {
	type Output = Duration;

	fn add(self, rhs: DurationDelta) -> Self::Output {
		let positive = rhs.nanos.is_positive();
		let secs = rhs.nanos.abs() / 1_000_000_000;
		let nanos = rhs.nanos.abs() % 1_000_000_000;
		let abs = Duration::new(secs as u64, nanos as u32);
		if positive {
			self + abs
		} else {
			self - abs
		}
	}
}

#[derive(Component, Default, Debug, Resource, Deref, DerefMut)]
pub struct Prev<T>(pub T);

impl<T: Clone> Prev<T> {
	pub fn update_component(
		mut cmds: Commands,
		mut q: Query<(Entity, &T, Option<&mut Self>), Changed<T>>,
	) where
		T: Component,
	{
		for (id, curr, prev) in &mut q {
			if let Some(mut prev) = prev {
				**prev = curr.clone();
			} else {
				cmds.entity(id).insert(Self(curr.clone()));
			}
		}
	}

	pub fn update_res(mut cmds: Commands, curr: Res<T>, prev: Option<ResMut<Self>>)
	where
		T: Resource,
	{
		if curr.is_changed() {
			if let Some(mut prev) = prev {
				**prev = curr.clone();
			} else {
				cmds.insert_resource(Self(curr.clone()));
			}
		}
	}
}

#[derive(Serialize, Deserialize, Copy, Clone, Debug, PartialEq, Reflect)]
pub enum Angle {
	Deg(f32),
	Rad(f32),
	PiOver(f32),
	TauOver(f32),
}

impl Angle {
	pub fn to_rad(self) -> Self {
		Angle::Rad(self.rad())
	}

	pub fn to_deg(self) -> Self {
		Angle::Deg(self.deg())
	}

	pub fn rad(self) -> f32 {
		use Angle::*;
		match self {
			Deg(deg) => deg * (TAU / 360.0),
			Rad(rad) => rad,
			PiOver(denom) => PI / denom,
			TauOver(denom) => TAU / denom,
		}
	}

	pub fn deg(self) -> f32 {
		use Angle::*;
		match self {
			Deg(deg) => deg,
			Rad(rad) => rad * (360.0 / TAU),
			PiOver(denom) => (PI / denom) * (360.0 / TAU),
			TauOver(denom) => (TAU / denom) * (360.0 / TAU),
		}
	}
}

impl Default for Angle {
	fn default() -> Self {
		Self::Rad(0.0)
	}
}

#[derive(Component, Copy, Clone, Debug)]
pub enum Target {
	RelativeTo { entity: Entity, relative: Transform },
	Local(Transform),
	Global(GlobalTransform),
}

impl Target {
	pub fn global(
		self,
		this_entity: Entity,
		global_xforms: &Query<&GlobalTransform>,
	) -> Result<GlobalTransform, QueryEntityError> {
		Ok(match self {
			Target::RelativeTo { entity, relative } => *global_xforms.get(entity)? * relative,
			Target::Local(local) => *global_xforms.get(this_entity)? * local,
			Target::Global(global) => global,
		})
	}
}

pub trait Easings {
	fn smoothstep(self) -> Self;
	fn smoothstep_clamped(self) -> Self;
	fn smootherstep(self) -> Self;
	fn smootherstep_clamped(self) -> Self;
	fn ease_in_cubic(self) -> Self;
	fn ease_out_cubic(self) -> Self;
}

macro_rules! impl_easings {
	($T:ty) => {
		impl Easings for $T {
			#[inline]
			fn smoothstep(self) -> Self {
				self * self * (3.0 - (2.0 * self))
			}

			#[inline]
			fn smoothstep_clamped(self) -> Self {
				self.clamp(0.0, 1.0).smoothstep()
			}

			#[inline]
			fn smootherstep(self) -> Self {
				self * self * self * (self * (6.0 * self - 15.0) + 10.0)
			}

			#[inline]
			fn smootherstep_clamped(self) -> Self {
				self.clamp(0.0, 1.0).smootherstep()
			}

			#[inline]
			fn ease_in_cubic(self) -> Self {
				self * self * self
			}

			#[inline]
			fn ease_out_cubic(self) -> Self {
				let x = 1.0 - self;
				1.0 - (x * x * x)
			}
		}
	};
}

impl_easings!(f32);
impl_easings!(f64);

#[derive(Resource, Clone, Debug, Deref, DerefMut)]
pub struct StateStack<S>(pub Vec<S>);

impl<S: FromWorld> FromWorld for StateStack<S> {
	fn from_world(world: &mut World) -> Self {
		Self(vec![S::from_world(world)])
	}
}

pub fn set_state_to_top_of_stack<S: States>(
	mut stack: ResMut<StateStack<S>>,
	curr: Res<State<S>>,
	mut next: ResMut<NextState<S>>,
) {
	if stack.last().is_none() {
		error!("State stack should not be empty. Pushing current state.");
		stack.push(curr.get().clone());
	}
	let top = stack.last().unwrap();
	if **curr != *top {
		next.0 = Some(top.clone());
	}
}

pub trait AppExt {
	fn insert_state_stack<S: States + Clone>(&mut self, init: S) -> &mut Self;
	fn init_state_stack<S: States + FromWorld>(&mut self) -> &mut Self;
}

impl AppExt for App {
	fn insert_state_stack<S: States + Clone>(&mut self, init: S) -> &mut Self {
		self.insert_state::<S>(init.clone())
			.insert_resource(StateStack(vec![init]))
			.add_systems(
				StateTransition,
				set_state_to_top_of_stack::<S>.before(run_enter_schedule::<S>),
			)
	}

	fn init_state_stack<S: States + FromWorld>(&mut self) -> &mut Self {
		self.init_state::<S>()
			.init_resource::<StateStack<S>>()
			.add_systems(
				StateTransition,
				set_state_to_top_of_stack::<S>.before(run_enter_schedule::<S>),
			)
	}
}
