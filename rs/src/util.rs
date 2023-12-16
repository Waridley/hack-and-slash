use bevy::{
	ecs::{
		event::Event,
		system::{EntityCommands, StaticSystemParam, SystemParam, SystemParamItem},
	},
	prelude::*,
};
use std::ops::{Add, Mul, Sub};

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
