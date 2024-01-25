use crate::util::{Diff, Lerp, LerpSlerp};
use bevy::{
	ecs::{query::QueryItem, system::EntityCommands},
	prelude::*,
	utils::HashMap,
};
use std::{
	ops::{Add, Deref, DerefMut, Mul},
	time::Duration,
};

pub trait CurveMut<T: Component>: Send + Sync + 'static {
	fn tick_mut(&mut self, val: QueryItem<&'static mut T>, t: Res<Time>) -> IsFinished;
	fn chain<B: CurveMut<T> + Sized>(self, other: B) -> ChainCurve<Self, B>
	where
		Self: Sized,
	{
		use IsFinished::*;
		ChainCurve {
			a: self,
			b: other,
			a_finished: No,
			b_finished: No,
		}
	}
}

type DynCurveMutFn<T> =
	dyn FnMut(QueryItem<&'static mut T>, Res<Time>) -> IsFinished + Send + Sync + 'static;

impl<T: Component> CurveMut<T> for DynCurveMutFn<T> {
	fn tick_mut(&mut self, val: QueryItem<&'static mut T>, t: Res<Time>) -> IsFinished {
		self(val, t)
	}
}

pub trait BlendableCurve<T: Component, D: 'static = T, P = f32>: Send + Sync + 'static {
	fn tick(&mut self, val: QueryItem<Ref<'static, T>>, t: Res<Time>) -> BlendableResult<D, P>;
	fn chain<B: BlendableCurve<T, D, P> + Sized>(self, other: B) -> ChainCurve<Self, B>
	where
		Self: Sized,
	{
		use IsFinished::*;
		ChainCurve {
			a: self,
			b: other,
			a_finished: No,
			b_finished: No,
		}
	}
}

type DynBlendableCurveFn<T, D = T, P = f32> = dyn FnMut(QueryItem<Ref<'static, T>>, Res<Time>) -> BlendableResult<D, P>
	+ Send
	+ Sync
	+ 'static;

impl<T: Component, D: 'static, P: 'static> BlendableCurve<T, D, P>
	for DynBlendableCurveFn<T, D, P>
{
	fn tick(&mut self, val: QueryItem<Ref<'static, T>>, t: Res<Time>) -> BlendableResult<D, P> {
		self(val, t)
	}
}

#[derive(Copy, Clone, Debug)]
pub enum AnimationBlendCommand<Delta> {
	Add(Delta),
	SetRelative(Delta),
	NoChange,
}

#[derive(Copy, Clone, Default, Debug)]
#[repr(u8)]
pub enum IsFinished {
	Yes = true as u8,
	#[default]
	No = false as u8,
}

impl From<IsFinished> for bool {
	fn from(value: IsFinished) -> Self {
		// SAFETY: Enums have all the same guarantees as bool,
		//    `IsFinished` is marked as `#[repr(u8)],
		//    `bool` and `u8` both have size & alignment 1,
		//    and the 2 variants are only created from bool values,
		unsafe { std::mem::transmute(value) }
	}
}

impl From<bool> for IsFinished {
	fn from(value: bool) -> Self {
		// SAFETY: Enums have all the same guarantees as bool,
		//    `IsFinished` is marked as `#[repr(u8)],
		//    `bool` and `u8` both have size & alignment 1,
		//    and the 2 possible bool values are valid IsFinished values,
		unsafe { std::mem::transmute(value) }
	}
}

impl Deref for IsFinished {
	type Target = bool;

	fn deref(&self) -> &Self::Target {
		// SAFETY: Same as `From` impls
		unsafe { &*(self as *const Self as *const bool) }
	}
}

impl DerefMut for IsFinished {
	fn deref_mut(&mut self) -> &mut Self::Target {
		// SAFETY: Same as `From` impls
		unsafe { &mut *(self as *mut Self as *mut bool) }
	}
}

pub struct BlendableResult<D, P = f32> {
	pub command: AnimationBlendCommand<D>,
	pub progress: Progress<P>,
	pub finished: IsFinished,
}

#[derive(Copy, Clone, Debug)]
pub enum Progress<Repr = f32> {
	/// The fraction of the full animation that has currently completed.
	///   - `0.0` represents an animation in its starting state.
	///   - `1.0` represents an animation that has exhausted its entire defined duration. `IsFinished` may still
	///     be `No` if, for example, the animation is looping.
	///   - Values outside the range `0.0..=1.0` may represent extrapolated operations.
	Fraction(Repr),
	/// The animation has an indefinite duration and thus it is impossible to measure progress.
	Indefinite,
}

#[derive(Debug, Clone)]
pub struct LerpCurve<T> {
	pub start: T,
	pub end: T,
	pub duration: Duration,
	pub curr_t: f32,
}

impl<T> LerpCurve<T> {
	pub fn new(start: T, end: T, duration: Duration) -> Self {
		Self {
			start,
			end,
			duration,
			curr_t: 0.0,
		}
	}
}

impl<T> CurveMut<T> for LerpCurve<T>
where
	T: Component + Lerp<T, f32, Output = T> + Clone + PartialEq + Send + Sync + 'static,
{
	fn tick_mut(&mut self, mut val: QueryItem<&'static mut T>, t: Res<Time>) -> IsFinished {
		let dur = self.duration.as_secs_f32();
		let t = f32::min(dur, self.curr_t + t.delta_seconds());
		self.curr_t = t;
		let new = self.start.clone().lerp(self.end.clone(), t);
		if *val != new {
			*val = new;
		}
		(t == dur).into()
	}
}

impl<T, D: 'static> BlendableCurve<T, D> for LerpCurve<T>
where
	T: Component
		+ Lerp<T, Output = T>
		+ Diff<Delta = D>
		+ Clone
		+ PartialEq
		+ Send
		+ Sync
		+ 'static,
{
	fn tick(&mut self, val: QueryItem<Ref<'static, T>>, t: Res<Time>) -> BlendableResult<D> {
		let dur = self.duration.as_secs_f32();
		let t = f32::min(dur, self.curr_t + t.delta_seconds());
		self.curr_t = t;
		let new = self.start.clone().lerp(self.end.clone(), t);
		let delta = new.relative_to(&val);
		BlendableResult {
			command: AnimationBlendCommand::SetRelative(delta),
			progress: Progress::Fraction(t / dur),
			finished: (t == dur).into(),
		}
	}
}

#[derive(Debug, Clone)]
pub struct LerpSlerpCurve<T> {
	pub start: T,
	pub end: T,
	pub duration: Duration,
	pub curr_t: f32,
}

impl<T> LerpSlerpCurve<T> {
	pub fn new(start: T, end: T, duration: Duration) -> Self {
		Self {
			start,
			end,
			duration,
			curr_t: 0.0,
		}
	}
}

impl<T: Component + LerpSlerp + PartialEq + Clone> CurveMut<T> for LerpSlerpCurve<T> {
	fn tick_mut(&mut self, mut val: QueryItem<&'static mut T>, t: Res<Time>) -> IsFinished {
		let t = f32::min(self.duration.as_secs_f32(), self.curr_t + t.delta_seconds());
		self.curr_t = t;
		let new = self.start.clone().lerp_slerp(self.end.clone(), t);
		if *val != new {
			*val = new;
		}
		(*val == self.end).into()
	}
}

impl BlendableCurve<Transform, TransformDelta> for LerpSlerpCurve<Transform> {
	fn tick(
		&mut self,
		val: QueryItem<Ref<'static, Transform>>,
		t: Res<Time>,
	) -> BlendableResult<TransformDelta> {
		let dur = self.duration.as_secs_f32();
		let t = f32::min(dur, self.curr_t + t.delta_seconds());
		self.curr_t = t;
		let new = self.start.lerp_slerp(self.end, t);
		let delta = new.relative_to(&val);
		BlendableResult {
			command: AnimationBlendCommand::SetRelative(delta),
			progress: Progress::Fraction(t / dur),
			finished: (t == dur).into(),
		}
	}
}

#[derive(Debug, Clone)]
pub struct LerpTo<T> {
	pub end: T,
	pub duration: Duration,
	pub curr_t: f32,
}

impl<T> LerpTo<T> {
	pub fn new(end: T, duration: Duration) -> Self {
		Self {
			end,
			duration,
			curr_t: 0.0,
		}
	}
}

impl<T: Component + Lerp<T, Output = T> + PartialEq + Clone> CurveMut<T> for LerpTo<T> {
	fn tick_mut(&mut self, mut val: QueryItem<&'static mut T>, t: Res<Time>) -> IsFinished {
		let dur = self.duration.as_secs_f32();
		let t = f32::min(dur, self.curr_t + t.delta_seconds());
		self.curr_t = t;
		let new = val.clone().lerp(self.end.clone(), t);
		if *val != new {
			*val = new;
		}
		(t == dur).into()
	}
}

impl<T, D: 'static> BlendableCurve<T, D> for LerpTo<T>
where
	T: Component
		+ Lerp<T, Output = T>
		+ Diff<Delta = D>
		+ Clone
		+ PartialEq
		+ Send
		+ Sync
		+ 'static,
{
	fn tick(&mut self, val: QueryItem<Ref<'static, T>>, t: Res<Time>) -> BlendableResult<D> {
		let dur = self.duration.as_secs_f32();
		let t = f32::min(dur, self.curr_t + t.delta_seconds());
		self.curr_t = t;
		let new = val.clone().lerp(self.end.clone(), t);
		let delta = new.relative_to(&val);
		BlendableResult {
			command: AnimationBlendCommand::SetRelative(delta),
			progress: Progress::Fraction(t / dur),
			finished: (t == dur).into(),
		}
	}
}

#[derive(Debug, Clone)]
pub struct LerpSlerpTo<T> {
	pub end: T,
	pub duration: Duration,
	pub curr_t: f32,
}

impl<T> LerpSlerpTo<T> {
	pub fn new(end: T, duration: Duration) -> Self {
		Self {
			end,
			duration,
			curr_t: 0.0,
		}
	}
}

impl<T: Component + LerpSlerp + PartialEq + Clone> CurveMut<T> for LerpSlerpTo<T> {
	fn tick_mut(&mut self, mut val: QueryItem<&'static mut T>, t: Res<Time>) -> IsFinished {
		let t = f32::min(self.duration.as_secs_f32(), self.curr_t + t.delta_seconds());
		self.curr_t = t;
		let new = val.clone().lerp_slerp(self.end.clone(), t);
		if *val != new {
			*val = new;
		}
		(*val == self.end).into()
	}
}

impl BlendableCurve<Transform, TransformDelta> for LerpSlerpTo<Transform> {
	fn tick(
		&mut self,
		val: QueryItem<Ref<'static, Transform>>,
		t: Res<Time>,
	) -> BlendableResult<TransformDelta> {
		let dur = self.duration.as_secs_f32();
		let t = f32::min(dur, self.curr_t + t.delta_seconds());
		self.curr_t = t;
		let new = val.lerp_slerp(self.end, t);
		let delta = new.relative_to(&val);
		BlendableResult {
			command: AnimationBlendCommand::SetRelative(delta),
			progress: Progress::Fraction(t / dur),
			finished: (t == dur).into(),
		}
	}
}

#[derive(Debug, Clone)]
pub struct ChainCurve<A, B> {
	pub a: A,
	pub b: B,
	a_finished: IsFinished,
	b_finished: IsFinished,
}

impl<T: Component, A: CurveMut<T>, B: CurveMut<T>> CurveMut<T> for ChainCurve<A, B> {
	fn tick_mut(&mut self, val: QueryItem<&'static mut T>, t: Res<Time>) -> IsFinished {
		use IsFinished::*;
		if *self.b_finished {
			return Yes;
		}

		if *self.a_finished {
			self.b_finished = self.b.tick_mut(val, t);
		} else {
			self.a_finished = self.a.tick_mut(val, t);
		}

		No
	}
}

impl<T, D, A, B> BlendableCurve<T, D> for ChainCurve<A, B>
where
	T: Component,
	D: 'static,
	A: BlendableCurve<T, D>,
	B: BlendableCurve<T, D>,
{
	fn tick(&mut self, val: QueryItem<Ref<'static, T>>, t: Res<Time>) -> BlendableResult<D> {
		use IsFinished::*;
		if *self.a_finished {
			let BlendableResult {
				command,
				progress,
				finished,
			} = self.b.tick(val, t);
			self.b_finished = finished;
			BlendableResult {
				command,
				progress: match progress {
					Progress::Fraction(t) => Progress::Fraction((t * 0.5) + 0.5),
					Progress::Indefinite => Progress::Indefinite,
				},
				finished,
			}
		} else {
			let BlendableResult {
				command,
				progress,
				finished,
			} = self.a.tick(val, t);
			self.a_finished = finished;
			BlendableResult {
				command,
				progress: match progress {
					Progress::Fraction(t) => Progress::Fraction(t * 0.5),
					Progress::Indefinite => {
						// TODO: This warning may result in false positives. Perhaps duration should be a separate check,
						//    so progress can be indefinite yet the duration known to be finite.
						#[cfg(debug_assertions)]
						bevy::log::warn!("First animation in chain has indefinite duration. The second may never run.");
						Progress::Indefinite
					}
				},
				finished: No,
			}
		}
	}
}

#[derive(Component)]
pub struct MutAnimation<T: Component> {
	pub entity: Entity,
	pub curve: Box<dyn CurveMut<T>>,
}

impl<T: Component + 'static> MutAnimation<T> {
	pub fn new(entity: Entity, tick: impl CurveMut<T>) -> Self {
		Self {
			entity,
			curve: Box::new(tick),
		}
	}

	/// System for running all mutating animations for a given component on a given entity.
	///
	/// # WARNING
	/// There is no guarantee in what order animations will be applied. Usually, you will want
	/// to manually make sure only one animation is actually spawned at a given time.
	pub fn animate(
		mut cmds: Commands,
		mut this_q: Query<(Entity, &mut Self)>,
		mut target_q: Query<&mut T>,
		time: Res<Time>,
	) {
		for (this_id, mut this) in &mut this_q {
			let Self { entity, curve } = &mut *this;
			match target_q.get_mut(*entity) {
				Ok(target) => {
					let finished = curve.tick_mut(target, Res::clone(&time));
					if *finished {
						cmds.entity(this_id).despawn()
					}
				}
				Err(e) => bevy::log::error!("{e}"),
			}
		}
	}
}

#[derive(Component)]
pub struct BlendableAnimation<T: Component, Delta> {
	pub entity: Entity,
	pub curve: Box<dyn BlendableCurve<T, Delta>>,
}

impl<T, D> BlendableAnimation<T, D>
where
	T: Component + Add<D, Output = T> + PartialEq + Clone,
	D: Add<D, Output = D> + Mul<f32, Output = D> + 'static,
{
	pub fn new(entity: Entity, tick: impl BlendableCurve<T, D>) -> Self {
		Self {
			entity,
			curve: Box::new(tick),
		}
	}

	/// System for blending all animations for a given component for a given entity.
	pub fn animate(
		mut cmds: Commands,
		mut this_q: Query<(Entity, &mut Self)>,
		mut target_q: Query<&mut T>,
		time: Res<Time>,
	) {
		use AnimationBlendCommand::*;

		let mut acc = HashMap::<Entity, D>::new();
		let mut to_blend = HashMap::<Entity, Vec<_>>::new();

		for (this_id, mut this) in &mut this_q {
			let Self { entity, curve } = &mut *this;
			match target_q.get_mut(*entity) {
				Ok(target) => {
					let BlendableResult {
						command,
						progress,
						finished,
					} = curve.tick(target.into(), Res::clone(&time));
					match command {
						Add(delta) => {
							let delta = match acc.remove(entity) {
								Some(curr) => curr + delta,
								None => delta,
							};
							acc.insert(*entity, delta);
						}
						SetRelative(value) => to_blend
							.entry(*entity)
							.or_insert_with(|| Vec::with_capacity(1))
							.push((progress, value)),
						NoChange => {}
					}
					if *finished {
						cmds.entity(this_id).despawn()
					}
				}
				Err(e) => bevy::log::error!("{e}"),
			}
		}

		for (id, mut to_blend) in to_blend.drain() {
			let mut val = target_q.get_mut(id).unwrap();
			let coef = 1.0 / to_blend.len() as f32;
			let mut sum_progress = 0.0;
			let num_fractions = to_blend
				.iter()
				.filter(|(progress, _)| match progress {
					Progress::Fraction(progress) => {
						sum_progress += *progress;
						false
					}
					Progress::Indefinite => true,
				})
				.count();
			let avg_progress = if num_fractions == 0 {
				coef
			} else {
				sum_progress / num_fractions as f32
			};
			let blended_offsets = to_blend
				.drain(..)
				.map(|(progress, value)| {
					let coef = match progress {
						Progress::Fraction(progress) => coef * (progress / avg_progress),
						Progress::Indefinite => coef,
					};
					value * coef
				})
				.reduce(|acc, item| acc + item)
				.expect("At least 1 item should exist if this entry in the HasMap exists");
			let new = val.clone()
				+ if let Some(acc) = acc.remove(&id) {
					blended_offsets + acc
				} else {
					blended_offsets
				};
			if *val != new {
				*val = new;
			}
		}
	}
}

pub trait StartAnimation<'w, 's, 'a> {
	fn start_mut_animation<T: Component>(
		&'a mut self,
		tick: impl CurveMut<T>,
	) -> EntityCommands<'w, 's, 'a>;
	fn start_blendable_animation<
		T: Component + Add<D, Output = T> + PartialEq + Clone,
		D: Add<Output = D> + Mul<f32, Output = D> + 'static,
	>(
		&'a mut self,
		tick: impl BlendableCurve<T, D>,
	) -> EntityCommands<'w, 's, 'a>;
}

impl<'w, 's, 'a> StartAnimation<'w, 's, 'a> for EntityCommands<'w, 's, 'a> {
	fn start_mut_animation<T: Component>(
		&'a mut self,
		tick: impl CurveMut<T>,
	) -> EntityCommands<'w, 's, 'a> {
		let id = self.id();
		self.commands().spawn(MutAnimation::new(id, tick))
	}

	fn start_blendable_animation<
		T: Component + Add<D, Output = T> + PartialEq + Clone,
		D: Add<Output = D> + Mul<f32, Output = D> + 'static,
	>(
		&'a mut self,
		tick: impl BlendableCurve<T, D>,
	) -> EntityCommands<'w, 's, 'a> {
		let id = self.id();
		self.commands().spawn(BlendableAnimation::new(id, tick))
	}
}

#[derive(Copy, Clone, Debug)]
pub struct TransformDelta(Transform);

impl Diff for Transform {
	type Delta = TransformDelta;

	fn relative_to(&self, rhs: &Self) -> Self::Delta {
		TransformDelta(Transform {
			translation: rhs.translation - self.translation,
			rotation: self.rotation.inverse() * rhs.rotation,
			scale: rhs.scale - self.scale,
		})
	}
}

impl Default for TransformDelta {
	fn default() -> Self {
		Self(Transform {
			scale: default(),
			..default()
		})
	}
}

impl Add for TransformDelta {
	type Output = Self;

	fn add(self, rhs: Self) -> Self::Output {
		Self(Transform {
			translation: self.0.translation + rhs.0.translation,
			rotation: self.0.rotation * rhs.0.rotation,
			scale: self.0.scale + rhs.0.scale,
		})
	}
}

impl Add<TransformDelta> for Transform {
	type Output = Self;

	fn add(self, rhs: TransformDelta) -> Self::Output {
		Self {
			translation: self.translation + rhs.0.translation,
			rotation: self.rotation * rhs.0.rotation,
			scale: self.scale * Vec3::ONE + rhs.0.scale,
		}
	}
}

impl Mul<f32> for TransformDelta {
	type Output = Self;

	fn mul(self, rhs: f32) -> Self::Output {
		Self(Transform {
			translation: self.0.translation * rhs,
			rotation: Quat::IDENTITY.slerp(self.0.rotation, rhs),
			scale: self.0.scale * rhs,
		})
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn animation_api() {
		let _ = App::new()
			.add_systems(Startup, spawn_animations)
			.add_systems(
				Update,
				(
					MutAnimation::<Transform>::animate,
					BlendableAnimation::<Transform, TransformDelta>::animate
						.after(MutAnimation::<Transform>::animate),
				),
			);

		fn spawn_animations(mut cmds: Commands, mut q: Query<(Entity, &Transform)>) {
			for (id, xform) in &mut q {
				cmds.entity(id)
					.start_blendable_animation(LerpSlerpCurve::new(
						*xform,
						Transform {
							translation: Vec3::ONE,
							..default()
						},
						Duration::from_secs(1),
					));
			}
		}
	}
}
