use crate::ui::{UiMat, UiMatBuilder};
use bevy::render::render_resource::VertexFormat;
use bevy::utils::{HashMap, HashSet};
use bevy::{
	asset::{io::Reader, AssetLoader, LoadContext},
	ecs::{
		component::ComponentInfo,
		query::QueryFilter,
		system::{EntityCommands, StaticSystemParam, SystemId, SystemParam, SystemParamItem},
	},
	math::Dir2,
	prelude::*,
	reflect::{serde::TypedReflectDeserializer, TypeRegistration, Typed},
	render::{
		mesh::{Indices, PrimitiveTopology, VertexAttributeValues},
		render_asset::RenderAssetUsages,
	},
	scene::{SceneLoaderError, SceneLoaderError::RonSpannedError},
	state::state::FreelyMutableState,
};
use itertools::Itertools;
use num_traits::NumCast;
use ron::Error::InvalidValueForType;
use serde::{de::DeserializeSeed, Deserialize, Serialize};
use std::{
	cmp::Ordering,
	collections::VecDeque,
	f32::consts::{PI, TAU},
	hash::Hash,
	iter::Sum,
	marker::PhantomData,
	ops::{Add, Div, Index, IndexMut, Mul, Sub},
	sync::OnceLock,
	time::Duration,
};
use std::any::TypeId;
use bevy::asset::UntypedAssetId;
use bevy::ecs::component::ComponentId;
use bevy::ecs::world::DeferredWorld;
use tiny_bail::prelude::{c, r};

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

/// See [Lerp smoothing is broken — Freya Holmér](https://youtu.be/LSNQuFEDOyQ?si=5Nhoe4BP6J-xr0wT)
pub trait LerpSmoothing
where
	Self: Clone + Sized + Sub<Self>,
	<Self as Sub<Self>>::Output: Mul<f32>,
	<<Self as Sub<Self>>::Output as Mul<f32>>::Output: Add<Self, Output = Self>,
{
	/// Calculate the value after `dt` seconds if self is decaying towards `to` with exponential
	/// decay constant `λ`, which is equivalent to `ln(2) / t½` where `t½` is the half-life of self.
	#[inline(always)]
	fn exp_decay(self, to: Self, λ: f32, dt: f32) -> Self {
		let t = exp_decay_factor(λ, dt);
		((self - to.clone()) * t) + to
	}
	
	/// Calculate the value after `dt` seconds if self is decaying towards `to` with the given
	/// `half_life` (`t½`) in seconds.
	#[inline(always)]
	fn decay(self, to: Self, half_life: f32, dt: f32) -> Self {
		let t = decay_factor(dt, half_life);
		((self - to.clone()) * t) + to
	}
	
	/// Calculate the value after `dt` seconds if self is decaying towards `to` with the given
	/// remainder after 1 second.
	///
	/// This could be written as `self.lerp(to, rem_frac_after_1s.pow(dt))`, however since
	/// the `pow` function is significantly more expensive than `exp`, and `dt` changing every frame
	/// theoretically makes optimizations much more difficult, this is implemented in terms of
	/// [`exp_decay`](Self::exp_decay) instead, in hopes that `const` values for
	/// `rem_frac_after_1s` might have their `ln` calculated at compile time if `f32::ln` is
	/// ever made `const` or the compiler is able to make such an optimization automatically.
	///
	/// Still, [`decay`](Self::decay) and [`exp_decay`](Self::exp_decay)
	/// should be preferred if at all convenient.
	#[inline(always)]
	fn frac_decay(self, to: Self, rem_frac_after_1s: f32, dt: f32) -> Self {
		to.exp_decay(self, -rem_frac_after_1s.ln(), dt)
	}
}

/// The lerp factor for [LerpSmoothing::exp_decay]. Useful if you need to use a non-standard lerp
/// function, like [Quat::slerp] for example.
///
/// <u>**Note</u>:**
/// If the intent is to lerp smooth from `a` to `b`, this factor needs to either be used to lerp
/// from `b` to `a` instead (`to.lerp(from, factor)`), or be subtracted from `1.0`,
/// e.g. `from.lerp(to, 1.0 - factor)`.
#[inline(always)]
pub fn exp_decay_factor(λ: f32, dt: f32) -> f32 {
	(-λ * dt).exp()
}

/// The lerp factor for [LerpSmoothing::decay]. Useful if you need to use a non-standard lerp
/// function, like [Quat::slerp] for example.
///
/// <u>**Note</u>:**
/// If the intent is to lerp smooth from `a` to `b`, this factor needs to either be used to lerp
/// from `b` to `a` instead (`to.lerp(from, factor)`), or be subtracted from `1.0`,
/// e.g. `from.lerp(to, 1.0 - factor)`.
#[inline(always)]
pub fn decay_factor(half_life: f32, dt: f32) -> f32 {
	(-dt / half_life).exp2()
}

/// The lerp factor for [LerpSmoothing::frac_decay]. Useful if you need to use a non-standard lerp
/// function, like [Quat::slerp] for example.
///
/// <u>**Note</u>:**
/// If the intent is to lerp smooth from `a` to `b`, this factor needs to either be used to lerp
/// from `b` to `a` instead (`to.lerp(from, factor)`), or be subtracted from `1.0`,
/// e.g. `from.lerp(to, 1.0 - factor)`.
#[inline(always)]
pub fn frac_decay_factor(rem_frac_after_1s: f32, dt: f32) -> f32 {
	exp_decay_factor(-rem_frac_after_1s.ln(), dt)
}

impl<T> LerpSmoothing for T
where
	T: Clone + Sized + Sub<T>,
	<T as Sub<T>>::Output: Mul<f32>,
	<<T as Sub<T>>::Output as Mul<f32>>::Output: Add<T, Output = T>, {}

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

	async fn load(
		&self,
		reader: &mut dyn Reader,
		_settings: &Self::Settings,
		_load_context: &mut LoadContext<'_>,
	) -> Result<Self::Asset, Self::Error> {
		let registration = self.registration.clone();
		let registry = self.registry.clone();
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
	}

	fn extensions(&self) -> &[&str] {
		&self.extensions
	}
}

pub trait LerpSlerp: Sized {
	fn lerp_slerp(self, rhs: Self, t: f32) -> Self;
	
	/// Spherical version of [LerpSmoothing::exp_decay].
	#[inline(always)]
	fn sph_exp_decay(self, to: Self, λ: f32, dt: f32) -> Self {
		let t = exp_decay_factor(λ, dt);
		to.lerp_slerp(self, t)
	}
	
	/// Spherical version of [LerpSmoothing::decay].
	#[inline(always)]
	fn sph_decay(self, to: Self, half_life: f32, dt: f32) -> Self {
		let t = decay_factor(half_life, dt);
		to.lerp_slerp(self, t)
	}
	
	/// Spherical version of [LerpSmoothing::frac_decay].
	#[inline(always)]
	fn sph_frac_decay(self, to: Self, rem_frac_after_1s: f32, dt: f32) -> Self {
		let t = frac_decay_factor(rem_frac_after_1s, dt);
		to.lerp_slerp(self, t)
	}
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

#[derive(Component, Default, Debug, Copy, Clone, Resource, Deref, DerefMut)]
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
	pub const ZERO: Self = Self::Rad(0.0);

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
		Self::ZERO
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
		global_xforms: Query<&GlobalTransform>,
	) -> Option<GlobalTransform> {
		// FIXME: Can't get lifetimes right for returning a Result.
		//    Compiler insists the error uses 'w no matter what I do
		Some(match self {
			Target::RelativeTo { entity, relative } => match global_xforms.get(entity) {
				Ok(xform) => *xform * relative,
				Err(_) => return None,
				// Err(e @ QueryEntityError::QueryDoesNotMatch(..)) => return Err(Box::new(QueryMismatch(entity, format!("{e}")))),
				// Err(QueryEntityError::NoSuchEntity(e)) => return Err(Box::new(QueryEntityError::<'static>::NoSuchEntity(e))),
				// Err(QueryEntityError::AliasedMutability(e)) => return Err(Box::new(QueryEntityError::<'static>::AliasedMutability(e))),
			},
			Target::Local(local) => match global_xforms.get(this_entity) {
				Ok(xform) => *xform * local,
				Err(_) => return None,
				// Err(e @ QueryEntityError::QueryDoesNotMatch(..)) => return Err(Box::new(QueryMismatch(this_entity, format!("{e}")))),
				// Err(QueryEntityError::NoSuchEntity(e)) => return Err(Box::new(QueryEntityError::<'static>::NoSuchEntity(e))),
				// Err(QueryEntityError::AliasedMutability(e)) => return Err(Box::new(QueryEntityError::<'static>::AliasedMutability(e))),
			},
			Target::Global(global) => global,
		})
	}
}

// #[derive(Debug)]
// struct QueryMismatch(Entity, String);
//
// impl std::fmt::Display for QueryMismatch {
// 	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
// 		self.1.fmt(f)
// 	}
// }
//
// impl std::error::Error for QueryMismatch {}

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

pub fn set_state_to_top_of_stack<S: FreelyMutableState>(
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
		*next = NextState::Pending(top.clone());
	}
}

pub trait AppExt {
	fn insert_state_stack<S: FreelyMutableState + Clone>(&mut self, init: S) -> &mut Self;
	fn init_state_stack<S: FreelyMutableState + FromWorld>(&mut self) -> &mut Self;
}

impl AppExt for App {
	fn insert_state_stack<S: FreelyMutableState + Clone>(&mut self, init: S) -> &mut Self {
		self.insert_state::<S>(init.clone())
			.insert_resource(StateStack(vec![init]))
			.add_systems(
				StateTransition,
				set_state_to_top_of_stack::<S>.before(EnterSchedules::<S>::default()),
			)
	}

	fn init_state_stack<S: FreelyMutableState + FromWorld>(&mut self) -> &mut Self {
		self.init_state::<S>()
			.init_resource::<StateStack<S>>()
			.add_systems(
				StateTransition,
				set_state_to_top_of_stack::<S>.before(EnterSchedules::<S>::default()),
			)
	}
}

/// Run condition that only returns true iff the given state is somewhere on the state stack.
pub fn state_on_stack<S: States>(state: S) -> impl FnMut(Res<StateStack<S>>) -> bool {
	move |stack: Res<StateStack<S>>| stack.contains(&state)
}

#[cfg_attr(not(target_arch = "wasm32"), inline(always))]
pub fn host_is_windows() -> bool {
	#[cfg(target_arch = "wasm32")]
	if let Some(window) = web_sys::window() {
		if let Ok(platform) = window.navigator().platform().as_deref() {
			if platform.starts_with("Win") {
				return true;
			}
		}
	}
	cfg!(target_os = "windows")
}

#[cfg_attr(not(target_arch = "wasm32"), inline(always))]
pub fn host_is_mac() -> bool {
	#[cfg(target_arch = "wasm32")]
	if let Some(window) = web_sys::window() {
		if let Ok(platform) = window.navigator().platform().as_deref() {
			if platform.starts_with("Mac") {
				return true;
			}
		}
	}
	cfg!(target_os = "macos")
}

#[cfg_attr(not(target_arch = "wasm32"), inline(always))]
pub fn host_is_linux() -> bool {
	#[cfg(target_arch = "wasm32")]
	if let Some(window) = web_sys::window() {
		if let Ok(platform) = window.navigator().platform().as_deref() {
			if platform.starts_with("Linux") {
				return true;
			}
		}
	}
	cfg!(target_os = "linux")
}

/// Convert a type from Y-up to Z-up coordinates
pub trait ZUp {
	type Output;
	fn z_up(self) -> Self::Output;
}

/// Similar to the [bevy::render::mesh::Meshable] implementation but produces coordinates useful in Z-up space.
impl ZUp for Rectangle {
	type Output = Mesh;
	/// Similar to `<Rectangle as Meshable::mesh` but produces coordinates useful in Z-up space.
	fn z_up(self) -> Self::Output {
		let [hw, hh] = [self.half_size.x, self.half_size.y];
		let positions = vec![
			[hw, 0.0, hh],
			[-hw, 0.0, hh],
			[-hw, 0.0, -hh],
			[hw, 0.0, -hh],
		];
		let normals = vec![[0.0, -1.0, 0.0]; 4];
		let uvs = vec![[1.0, 0.0], [0.0, 0.0], [0.0, 1.0], [1.0, 1.0]];
		let indices = Indices::U32(vec![0, 1, 2, 0, 2, 3]);

		Mesh::new(
			PrimitiveTopology::TriangleList,
			RenderAssetUsages::default(),
		)
		.with_inserted_indices(indices)
		.with_inserted_attribute(Mesh::ATTRIBUTE_POSITION, positions)
		.with_inserted_attribute(Mesh::ATTRIBUTE_NORMAL, normals)
		.with_inserted_attribute(Mesh::ATTRIBUTE_UV_0, uvs)
	}
}

impl ZUp for Mesh {
	type Output = Self;

	fn z_up(mut self) -> Self::Output {
		for (id, attr) in self.attributes_mut() {
			debug!("Converting attribute {id:?}");
			if let VertexAttributeValues::Float32x3(attr) = attr {
				for [_, y, z] in attr.iter_mut() {
					let oy = *y;
					*y = -*z;
					*z = oy;
				}
			}
		}
		self
	}
}

/// Run condition that matches on a state pattern instead of using `PartialEq`
///
///
/// ### Usage:
/// Due to limitations in `macro_rules` syntax, this only works for enums,
/// and the type must be included in angle brackets.
/// ```
/// # fn in_baz() {}
/// # use bevy::prelude::*;
/// # use sond_has_engine::state_matches;
///
/// #[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default, States)]
/// enum Foo {
///     #[default]
///     Bar,
///     Baz(i8),
/// }
///
/// App::new().add_systems(
///     Update,
///     in_baz.run_if(state_matches!(<Foo>::Baz(_)))
/// );
/// ```
#[macro_export]
macro_rules! state_matches {
	(<$T:ty>::$pat:pat) => {
		|state: Res<State<$T>>| {
			use $T::*;
			matches!(state.get(), $pat)
		}
	};
}

/// Shorthand for spawning a tree of entities.
/// ```
/// # use bevy::prelude::*;
/// # use sond_has_engine::entity_tree;
/// # fn test_entity_tree_macro(mut commands: Commands) {
/// let root = entity_tree!(commands; (
///     // The bundles to spawn on the root entity
///     TransformBundle::default(),
///     VisibilityBundle::default();
///     #children: [
///         ( // First child
///             => |cmds| {
///                 // Do stuff with ChildBuilder before spawning this child
///                 dbg!(cmds.parent_entity());
///             };
///             // The bundles to spawn on the first child
///             TransformBundle {
///                 local: Transform::from_translation(Vec3::splat(3.0)),
///                 ..default()
///             },
///             VisibilityBundle::default();
///             => |cmds| {
///                 // Do stuff after spawning entity but before adding children
///                 dbg!(cmds.id());
///             }
///             #children: [(// grandchildren
///                 TransformBundle::default(),
///                 VisibilityBundle::default(),
///             )];
///             // You can repeat pairs of `=> |_| {}` and `#children: []`...
///             => |cmds| {
///                 // Do more stuff after spawning children
///             }
///             #children: [
///                 // Add even more children
///             ]
///             // ...indefinitely.
///             // After each semicolon, both the closure and `#children` sections are optional.
///         ),
///         ( // Second child
///             PbrBundle::default(),
///         ),
///         // ...etc.
///     ]
/// )).id();
/// # }
/// ```
#[macro_export]
macro_rules! entity_tree {
	(
		$cmds:ident;
		(
			$(=> |$first_cmds:ident| $first:block ;)?
			$($bundles:expr),* $(,)?
			$(;
				$(=> |$then_cmds:ident| $then:block)?
				$(#children: [ $($children:tt),* $(,)? ])?
			)*
		)
	) => {
		{
			$({
				let $first_cmds = &mut $cmds;
				$first;
			};)?
			#[allow(unused_mut)]
			let mut $cmds = $cmds.spawn((
		    $($bundles),*
		  ));
			$(
				$({
					let $then_cmds = &mut $cmds;
					$then;
				};)?
				#[allow(unused_mut)]
				$($cmds.with_children(|mut $cmds| {
			    $(entity_tree!($cmds; $children);)*
			  });)?
			)*
			$cmds
		}
	}
}

#[macro_export]
macro_rules! todo_warn {
	() => {
		::bevy::log::warn!("not yet implemented")
	};
	($($arg:tt)+) => {
		::bevy::log::warn!("not yet implemented: {}", format_args!($($arg)+))
	};
}

#[macro_export]
macro_rules! todo_err {
	() => {
		::bevy::log::error!("not yet implemented")
	};
	($($arg:tt)+) => {
		::bevy::log::error!("not yet implemented: {}", format_args!($($arg)+))
	};
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize, Reflect)]
#[reflect(Serialize, Deserialize)]
#[repr(u8)]
pub enum CompassDirection {
	North,
	NorthEast,
	East,
	SouthEast,
	South,
	SouthWest,
	West,
	NorthWest,
}

impl CompassDirection {
	pub const N: Self = Self::North;
	pub const NE: Self = Self::NorthEast;
	pub const E: Self = Self::East;
	pub const SE: Self = Self::SouthEast;
	pub const S: Self = Self::South;
	pub const SW: Self = Self::SouthWest;
	pub const W: Self = Self::West;
	pub const NW: Self = Self::NorthWest;

	///  All directions in clockwise order starting at `North`
	pub const CLOCKWISE: [Self; 8] = [
		Self::N,
		Self::NE,
		Self::E,
		Self::SE,
		Self::S,
		Self::SW,
		Self::W,
		Self::NW,
	];

	/// All directions in trigonometric order -- counter-clockwise starting at `East`
	pub const TRIG_ORDER: [Self; 8] = [
		Self::E,
		Self::NE,
		Self::N,
		Self::NW,
		Self::W,
		Self::SW,
		Self::S,
		Self::SE,
	];

	#[inline]
	pub fn direction(self) -> Dir2 {
		use std::f32::consts::FRAC_1_SQRT_2;
		match self {
			Self::North => Dir2::Y,
			Self::NorthEast => Dir2::new_unchecked(Vec2::new(FRAC_1_SQRT_2, FRAC_1_SQRT_2)),
			Self::East => Dir2::X,
			Self::SouthEast => Dir2::new_unchecked(Vec2::new(FRAC_1_SQRT_2, -FRAC_1_SQRT_2)),
			Self::South => Dir2::NEG_Y,
			Self::SouthWest => Dir2::new_unchecked(Vec2::new(-FRAC_1_SQRT_2, -FRAC_1_SQRT_2)),
			Self::West => Dir2::NEG_X,
			Self::NorthWest => Dir2::new_unchecked(Vec2::new(-FRAC_1_SQRT_2, FRAC_1_SQRT_2)),
		}
	}

	/// Rotation around the origin in radians, starting at `East` (positive `x`)
	#[inline]
	pub const fn radians(self) -> f32 {
		use std::f32::consts::{FRAC_PI_2, FRAC_PI_4};
		const FRAC_PI_3_4: f32 = 2.35619449615478515625;
		const NEG_FRAC_PI_3_4: f32 = -2.35619449615478515625;
		const NEG_FRAC_PI_4: f32 = -0.785398163397448309615660845819875721;
		const NEG_FRAC_PI_2: f32 = -1.57079632679489661923132169163975144;
		match self {
			Self::North => FRAC_PI_2,
			Self::NorthEast => FRAC_PI_4,
			Self::East => 0.0,
			Self::SouthEast => NEG_FRAC_PI_4,
			Self::South => NEG_FRAC_PI_2,
			Self::SouthWest => NEG_FRAC_PI_3_4,
			Self::West => PI,
			Self::NorthWest => FRAC_PI_3_4,
		}
	}

	pub fn nearest_to_angle(radians: f32) -> Self {
		// Ensure positive
		let rot = radians + TAU;
		Self::TRIG_ORDER[((rot * 8.0) / TAU).round() as usize % 8]
	}

	pub fn nearest_to(direction: Vec2) -> Self {
		Self::nearest_to_angle(direction.to_angle())
	}
}

#[cfg(test)]
#[test]
fn test_compass_directions() {
	for dir in CompassDirection::CLOCKWISE {
		assert_eq!(dir, CompassDirection::nearest_to_angle(dir.radians()));
		assert_eq!(dir, CompassDirection::nearest_to(*dir.direction()));
	}
}

/// Convenience for computing flat normals
pub trait Flat {
	fn flat(self) -> Mesh;
}

impl<T: Into<Mesh>> Flat for T {
	fn flat(self) -> Mesh {
		self.into()
			.with_duplicated_vertices()
			.with_computed_flat_normals()
	}
}

#[derive(Component, Debug, Clone, Copy, Reflect)]
#[require(Transform, Visibility)]
pub struct MeshOutline {
	pub color: Color,
	pub offset: Vec3,
	pub thickness: Vec3,
	pub invert_faces: bool,
}

impl Default for MeshOutline {
	fn default() -> Self {
		Self {
			color: Color::BLACK,
			offset: Vec3::ZERO,
			thickness: Vec3::splat(0.05),
			invert_faces: true,
		}
	}
}

impl MeshOutline {
	pub fn default_material() -> UiMat {
		UiMatBuilder {
			std: StandardMaterial {
				// base_color: Color::BLACK,
				unlit: true,
				..default()
			},
			..default()
		}
		.build()
	}

	pub const fn from_thickness(thickness: f32) -> Self {
		Self {
			color: Color::BLACK,
			offset: Vec3::ZERO,
			thickness: Vec3::splat(thickness),
			invert_faces: true,
		}
	}

	pub const fn facing(direction: Dir3, thickness: f32, distance_behind_front: f32) -> Self {
		let direction = direction.as_vec3();
		Self {
			color: Color::BLACK,
			offset: Vec3 {
				x: direction.x  * -(thickness + distance_behind_front),
				y: direction.y  * -(thickness + distance_behind_front),
				z: direction.z  * -(thickness + distance_behind_front),
			},
			thickness: Vec3::splat(thickness),
			invert_faces: false,
		}
	}

	pub const fn front_facing(thickness: f32, distance_behind_front: f32) -> Self {
		Self::facing(Dir3::NEG_Y, thickness, distance_behind_front)
	}

	pub fn sync(
		mut cmds: Commands,
		parents: Query<Ref<Mesh3d>>,
		outlines: Query<(Entity, Ref<Self>, &Parent, Has<Mesh3d>)>,
		mut meshes: ResMut<Assets<Mesh>>,
		mut events: EventReader<AssetEvent<Mesh>>,
	) {
		let changed_meshes = events
			.read()
			.filter_map(|ev| match ev {
				AssetEvent::Modified { id } => Some(*id),
				_ => None,
			})
			.collect::<HashSet<_>>();

		for (id, this, parent, has_mesh) in &outlines {
			let parent_mesh = match parents.get(parent.get()) {
				Ok(parent_mesh) => parent_mesh,
				Err(e) => {
					error!(?id, "couldn't get parent: {e}");
					continue;
				}
			};
			if parent_mesh.is_changed()
				|| this.is_changed()
				|| !has_mesh || changed_meshes.contains(&parent_mesh.id())
			{
				let Some(parent_mesh) = meshes.get(parent_mesh.id()) else {
					error!(?parent_mesh, "couldn't get parent mesh");
					continue;
				};
				let outline_mesh = this.generate_for(parent_mesh);
				let outline_mesh = meshes.add(outline_mesh);
				cmds.entity(id).insert(Mesh3d(outline_mesh));
			}
		}
	}

	pub fn generate_for(&self, mesh: &Mesh) -> Mesh {
		let mut mesh = mesh.clone();
		deduplicate_vertices(&mut mesh, f32::EPSILON);

		let positions = mesh
			.attribute(Mesh::ATTRIBUTE_POSITION)
			.unwrap()
			.as_float3()
			.unwrap();

		let Some(indices) = mesh.indices() else {
			error!("was unable to deduplicate vertices");
			return mesh;
		};

		let mut edge_share_counts = HashMap::<(usize, usize), usize>::new();
		for mut tri in indices.iter().chunks(3).into_iter() {
			let [a, b, c] = [tri.next().unwrap(), tri.next().unwrap(), tri.next().unwrap()];
			// Sort for deterministic keys since order doesn't matter
			let ab = if a > b { (b, a) } else { (a, b) };
			let bc = if b > c { (c, b) } else { (b, c) };
			let ca = if c > a { (a, c) } else { (c, a) };
			*edge_share_counts.entry(ab).or_insert(0) += 1;
			*edge_share_counts.entry(bc).or_insert(0) += 1;
			*edge_share_counts.entry(ca).or_insert(0) += 1;
		}
		let mut watertight = true;
		let mut fin_found = false;
		for &count in edge_share_counts.values() {
			match count {
				0 => unreachable!(),
				1 => watertight = false,
				2 => (),
				_ => fin_found = true,
			}
		}
		
		if fin_found {
			warn!("Mesh has fins, outline will likely be messy");
		}
		
		if !watertight {
			// Mesh is likely just flat. We could warn here and have a specific option to solidify the
			// mesh, but currently I think auto-solidifying is the most useful option.
			
			let mut indices = indices.iter().collect::<Vec<_>>();
			let normals = mesh.attribute(Mesh::ATTRIBUTE_NORMAL).unwrap().as_float3().unwrap();
			let len = positions.len();
			let (mut front_verts, back_verts): (Vec<_>, Vec<_>) = positions.into_iter().enumerate().map(|(i, pos)| {
				let pos = Vec3::from_array(*pos);
				let norm = Vec3::from_array(normals[i]);
				((pos + norm).to_array(), (pos - norm).to_array())
			}).unzip();
			front_verts.extend(back_verts.into_iter());
			let mut verts = front_verts;
			
			let mut new_indices = indices
				.chunks_exact(3)
				.flat_map(|front| {
					[front[0] + len, front[2] + len, front[1] + len]
				})
				.collect::<Vec<_>>();
			
			for mut tri in indices.chunks_exact(3) {
				let [a, b, c] = [tri[0], tri[1], tri[2]];
				// Sort for deterministic keys since order doesn't matter
				let ab = if a > b { (b, a) } else { (a, b) };
				let bc = if b > c { (c, b) } else { (b, c) };
				let ca = if c > a { (a, c) } else { (c, a) };
				
				let mut add_side = |a, b| {
					let (c, d) = (a + len, b + len);
					new_indices.extend([b, a, c, b, c, d]);
				};
				
				if edge_share_counts[&ab] == 1 {
					add_side(a, b);
				}
				if edge_share_counts[&bc] == 1 {
					add_side(b, c);
				}
				if edge_share_counts[&ca] == 1 {
					add_side(c, a);
				}
			}
			let front_indices_len = indices.len();
			indices.extend(new_indices);
			
			let indices = if let Ok(_) = u16::try_from(len) {
				Indices::U16(indices.into_iter().map(|i| i as u16).collect())
			} else {
				Indices::U32(indices.into_iter().map(|i| i as u32).collect())
			};
			
			let mut normals = geometric_normals_impl(&verts, &indices);
			
			debug_assert_eq!(verts.len() % 2, 0, "we doubled the length by extending from back_verts");
			
			if self.invert_faces {
				// Normals are now correct, but we don't want to keep the solidified positions before applying offset.
				let (front_verts, back_verts) = verts.split_at_mut(len);
				front_verts.clone_from_slice(positions);
				back_verts.clone_from_slice(positions);
				mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, verts);
				mesh.insert_attribute(Mesh::ATTRIBUTE_NORMAL, normals);
				mesh.insert_indices(indices);
			} else {
				// Solidified mesh was needed for normal caculation, but not for final outline
				verts.truncate(len);
				verts.clone_from_slice(positions);
				normals.truncate(len);
				let indices = match indices {
					Indices::U16(mut indices) => {
						indices.truncate(front_indices_len);
						Indices::U16(indices)
					},
					Indices::U32(mut indices) => {
						indices.truncate(front_indices_len);
						Indices::U32(indices)
					},
				};
				mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, verts);
				mesh.insert_attribute(Mesh::ATTRIBUTE_NORMAL, normals);
				mesh.insert_indices(indices);
			}
		}
		
		// Have to re-get these because they may have changed in solidify above
		let positions = mesh
			.attribute(Mesh::ATTRIBUTE_POSITION)
			.unwrap()
			.as_float3()
			.unwrap();
		let indices = mesh.indices().unwrap();
		
		// The unnormalized normal directions of all triangles before moving vertices.
		// In small inside radiuses, verts can end up crossing each other and flipping the
		// orientation of faces. We simply detect when this happens and flip them back.
		let scaled_face_normals = indices
			.iter()
			.chunks(3)
			.into_iter()
			.map(|mut tri| {
				let a = Vec3::from_array(positions[tri.next().unwrap()]);
				let b = Vec3::from_array(positions[tri.next().unwrap()]);
				let c = Vec3::from_array(positions[tri.next().unwrap()]);
				let ab = b - a;
				let ac = c - a;
				// No need to normalize just to compare direction. Actually makes it more precise.
				ab.cross(ac)
			})
			.collect::<Vec<_>>();

		let Some(VertexAttributeValues::Float32x3(normals)) =
			mesh.attribute_mut(Mesh::ATTRIBUTE_NORMAL)
		else {
			unreachable!("normals were just computed");
		};

		let offsets = normals
			.iter_mut()
			.map(|norm| {
				let n = Vec3::from_array(*norm);
				if self.invert_faces {
					*norm = (-n).to_array();
				}
				(n * self.thickness) + self.offset
			})
			.collect::<Vec<_>>();

		let Some(VertexAttributeValues::Float32x3(positions)) =
			mesh.attribute_mut(Mesh::ATTRIBUTE_POSITION)
		else {
			unreachable!();
		};
		let len = positions.len();

		for (pos, offset) in positions.iter_mut().zip(offsets.into_iter()) {
			*pos = (Vec3::from_array(*pos) + offset).to_array()
		}

		let positions = positions
			.iter()
			.copied()
			.map(Vec3::from_array)
			.collect::<Vec<_>>();

		macro_rules! invert_windings {
			($indices:ident) => {
				for (i, tri) in $indices.chunks_exact_mut(3).enumerate() {
					let a = positions[tri[0] as usize];
					let b = positions[tri[1] as usize];
					let c = positions[tri[2] as usize];
					let ab = b - a;
					let ac = c - a;
					let prev_norm = scaled_face_normals[i];
					let norm = ab.cross(ac);
					let translation_flipped_face = prev_norm.dot(norm) >= 0.0;
					if !(self.invert_faces ^ translation_flipped_face) {
						tri.swap(0, 2)
					}
				}
			};
		}

		match mesh.indices_mut().unwrap() {
			Indices::U16(indices) => invert_windings!(indices),
			Indices::U32(indices) => invert_windings!(indices),
		}

		let colors = vec![self.color.to_linear().to_f32_array(); len];
		mesh.with_inserted_attribute(Mesh::ATTRIBUTE_COLOR, colors)
	}
}

pub fn deduplicate_vertices(mesh: &mut Mesh, max_diff: f32) {
	if mesh.primitive_topology() != PrimitiveTopology::TriangleList {
		error!(
			"Can't merge vertices of topology {:?}",
			mesh.primitive_topology()
		);
		return;
	}
	if mesh.indices().is_some() {
		debug!("Mesh is already merged");
		return;
	}

	let unmerged = match mesh.attribute(Mesh::ATTRIBUTE_POSITION) {
		Some(VertexAttributeValues::Float32x3(positions)) => positions
			.iter()
			.map(|pos| Vec3::from_array(*pos))
			.collect::<Vec<_>>(),
		Some(other) => {
			error!(
				"Expected {:?}, got {:?}",
				Mesh::ATTRIBUTE_POSITION,
				VertexFormat::from(other)
			);
			return;
		}
		None => {
			error!("Mesh is missing positions");
			return;
		}
	};

	let mut new_verts = Vec::<&Vec3>::new();
	let mut retain = vec![true; unmerged.len()];
	let mut indices = Indices::U16(Vec::with_capacity(unmerged.len()));

	for (i, orig) in unmerged.iter().enumerate() {
		let earlier = new_verts
			.iter()
			.copied()
			.enumerate()
			.find_map(|(i, earlier)| orig.abs_diff_eq(*earlier, max_diff).then_some(i));

		let i = if let Some(earlier) = earlier {
			retain[i] = false;
			earlier
		} else {
			let new_i = new_verts.len();
			new_verts.push(orig);
			new_i
		} as u32;
		indices.push(i);
	}
	debug_assert_eq!(indices.len(), unmerged.len());

	let new_verts = new_verts.into_iter().map(Vec3::to_array).collect();

	let had_tangents = mesh.remove_attribute(Mesh::ATTRIBUTE_TANGENT).is_some();

	for (_, values) in mesh.attributes_mut() {
		let mut i = 0;
		macro_rules! retain_values {
			($values:ident) => {
				$values.retain(|_| {
					let retain = retain[i];
					i += 1;
					retain
				})
			};
		}
		match values {
			VertexAttributeValues::Float32(values) => retain_values!(values),
			VertexAttributeValues::Sint32(values) => retain_values!(values),
			VertexAttributeValues::Uint32(values) => retain_values!(values),
			VertexAttributeValues::Float32x2(values) => retain_values!(values),
			VertexAttributeValues::Sint32x2(values) => retain_values!(values),
			VertexAttributeValues::Uint32x2(values) => retain_values!(values),
			VertexAttributeValues::Float32x3(values) => retain_values!(values),
			VertexAttributeValues::Sint32x3(values) => retain_values!(values),
			VertexAttributeValues::Uint32x3(values) => retain_values!(values),
			VertexAttributeValues::Float32x4(values) => retain_values!(values),
			VertexAttributeValues::Sint32x4(values) => retain_values!(values),
			VertexAttributeValues::Uint32x4(values) => retain_values!(values),
			VertexAttributeValues::Sint16x2(values) => retain_values!(values),
			VertexAttributeValues::Snorm16x2(values) => retain_values!(values),
			VertexAttributeValues::Uint16x2(values) => retain_values!(values),
			VertexAttributeValues::Unorm16x2(values) => retain_values!(values),
			VertexAttributeValues::Sint16x4(values) => retain_values!(values),
			VertexAttributeValues::Snorm16x4(values) => retain_values!(values),
			VertexAttributeValues::Uint16x4(values) => retain_values!(values),
			VertexAttributeValues::Unorm16x4(values) => retain_values!(values),
			VertexAttributeValues::Sint8x2(values) => retain_values!(values),
			VertexAttributeValues::Snorm8x2(values) => retain_values!(values),
			VertexAttributeValues::Uint8x2(values) => retain_values!(values),
			VertexAttributeValues::Unorm8x2(values) => retain_values!(values),
			VertexAttributeValues::Sint8x4(values) => retain_values!(values),
			VertexAttributeValues::Snorm8x4(values) => retain_values!(values),
			VertexAttributeValues::Uint8x4(values) => retain_values!(values),
			VertexAttributeValues::Unorm8x4(values) => retain_values!(values),
		}
	}
	mesh.insert_attribute(
		Mesh::ATTRIBUTE_POSITION,
		VertexAttributeValues::Float32x3(new_verts),
	);
	mesh.insert_indices(indices);
	compute_geometric_normals(mesh);
	if had_tangents {
		if let Err(e) = mesh.generate_tangents() {
			error!("{e}");
		}
	}
}

/// Like `Mesh::compute_smooth_normals` but does not let multiple co-planar triangles influence
/// the normal of a vertex disproportionately.
pub fn compute_geometric_normals(mesh: &mut Mesh) {
	let Some(VertexAttributeValues::Float32x3(positions)) =
		mesh.attribute(Mesh::ATTRIBUTE_POSITION)
	else {
		unreachable!();
	};
	let Some(indices) = mesh.indices() else {
		error!("Mesh is not indexed");
		return;
	};
	let normals = geometric_normals_impl(positions, indices);
	mesh.insert_attribute(Mesh::ATTRIBUTE_NORMAL, normals);
}

fn geometric_normals_impl(positions: &[[f32; 3]], indices: &Indices) -> Vec<Vec3> {
	let mut normals = vec![Vec3::ZERO; positions.len()];
	
	for mut tri in &indices.iter().chunks(3) {
		let [a, b, c] = [
			tri.next().unwrap(),
			tri.next().unwrap(),
			tri.next().unwrap(),
		];
		let pa = Vec3::from(positions[a]);
		let pb = Vec3::from(positions[b]);
		let pc = Vec3::from(positions[c]);
		let norm = Triangle3d::new(pa, pb, pc)
			.normal()
			.as_ref()
			.map(Dir3::as_vec3)
			.unwrap_or(Vec3::ZERO);
		
		let wa = (pb - pa).angle_between(pc - pa);
		let wb = (pa - pb).angle_between(pc - pb);
		let wc = (pa - pc).angle_between(pb - pc);
		
		normals[a] += norm * wa;
		normals[b] += norm * wb;
		normals[c] += norm * wc;
	}
	
	normals.iter_mut().for_each(|n| *n = n.normalize_or_zero());
	normals
}

#[derive(Component, Debug, Clone)]
#[component(on_insert = Self::on_insert)]
pub struct PendingErasedAsset(pub UntypedHandle);

impl PendingErasedAsset {
	pub fn on_insert(mut world: DeferredWorld, entity: Entity, _id: ComponentId) {
		let this = world.get::<Self>(entity).unwrap();
		let handle = this.0.clone();
		let mut downcasters = world.resource_mut::<ErasedAssetDowncasters>();
		let ty = handle.type_id();
		let mut downcaster = if let Some(mut replacer) = downcasters.0.remove(&ty) {
			replacer
		} else {
			error!("Missing downcaster for {handle:?}");
			return
		};
		let mut cmds = world.commands();
		let cmds = cmds.entity(entity);
		downcaster(cmds, handle);
		let mut downcasters = world.resource_mut::<ErasedAssetDowncasters>();
		downcasters.0.insert(ty, downcaster);
	}
}

pub type ErasedAssetDowncaster = Box<dyn FnMut(EntityCommands, UntypedHandle) + Send + Sync>;

#[derive(Resource, Default)]
pub struct ErasedAssetDowncasters(HashMap<TypeId, ErasedAssetDowncaster>);

impl ErasedAssetDowncasters {
	pub fn register<A: Asset>(&mut self, replacer: impl FnMut(EntityCommands, UntypedHandle) + Send + Sync + 'static) {
		if let Err(e) = self.0.try_insert(TypeId::of::<A>(), Box::new(replacer)) {
			panic!("Replacer already registered for {}", A::type_path());
		}
	}
}

pub trait RegisterUntypedAssetDowncaster {
	fn register_untyped_asset_downcaster<A: Asset>(&mut self, replacer: impl FnMut(EntityCommands, UntypedHandle) + Send + Sync + 'static) -> &mut Self;
}

impl RegisterUntypedAssetDowncaster for App {
	fn register_untyped_asset_downcaster<A: Asset>(&mut self, replacer: impl FnMut(EntityCommands, UntypedHandle) + Send + Sync + 'static) -> &mut Self {
		let mut replacers = self.world_mut().get_resource_or_init::<ErasedAssetDowncasters>();
		replacers.register::<A>(replacer);
		self
	}
}

pub fn downcast_material<M: Material>(
	mut cmds: EntityCommands,
	handle: UntypedHandle,
) {
	if let Ok(typed) = handle.try_typed::<M>() {
		cmds
			.remove::<PendingErasedAsset>()
			.insert(MeshMaterial3d(typed));
	}
}
