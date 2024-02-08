use crate::util::{Diff, Target};
use bevy::{
	ecs::{query::QueryEntityError, system::EntityCommands},
	prelude::*,
	transform::TransformSystem::TransformPropagate,
	utils::HashMap,
};
use std::{
	cmp::Ordering,
	fmt::{Debug, Formatter},
	hash::{Hash, Hasher},
	marker::PhantomData,
	ops::{Add, Mul},
	time::Duration,
};

#[derive(Debug)]
pub struct BuiltinAnimations;

impl Plugin for BuiltinAnimations {
	fn build(&self, app: &mut App) {
		app.add_plugins((
			AnimationPlugin::<Transform>::PLUGIN,
			AnimationPlugin::<GlobalTransform>::PLUGIN,
			AnimationPlugin::<Visibility>::PLUGIN,
			AnimationPlugin::<ViewVisibility>::PLUGIN,
		))
		.add_systems(
			PostUpdate,
			BlendTargets::animate
				.before(apply_animations::<Transform>)
				.in_set(AnimationSet::<Transform>::SET),
		)
		.configure_sets(
			PostUpdate,
			(
				AnimationSet::<Transform>::SET.before(TransformPropagate),
				AnimationSet::<GlobalTransform>::SET.before(TransformPropagate),
			),
		);
	}
}

#[derive(Default, Debug)]
pub struct AnimationPlugin<T: Component>(PhantomData<T>);

impl<T: Component> AnimationPlugin<T> {
	pub const PLUGIN: Self = Self(PhantomData::<T>);
}

impl<T: Component> Plugin for AnimationPlugin<T> {
	fn build(&self, app: &mut App) {
		app.add_event::<ComponentDelta<T>>().add_systems(
			PostUpdate,
			(DynAnimation::<T>::animate, apply_animations::<T>)
				.chain()
				.in_set(AnimationSet::<T>::default()),
		);
	}
}

#[derive(Default, Debug)]
pub struct ResAnimationPlugin<T: Resource>(PhantomData<T>);

impl<T: Resource> Plugin for ResAnimationPlugin<T> {
	fn build(&self, app: &mut App) {
		app.add_event::<Delta<T>>().add_systems(
			PostUpdate,
			(
				DynResAnimation::<T>::animate_res.before(apply_res_animations::<T>),
				apply_res_animations::<T>,
			)
				.in_set(AnimationSet::<T>::default()),
		);
	}
}

#[derive(SystemSet)]
pub struct AnimationSet<T>(PhantomData<T>);
impl<T> AnimationSet<T> {
	pub const SET: Self = Self(PhantomData);
}
impl<T> Copy for AnimationSet<T> {}
impl<T> Clone for AnimationSet<T> {
	fn clone(&self) -> Self {
		*self
	}
}
impl<T> Default for AnimationSet<T> {
	fn default() -> Self {
		Self(default())
	}
}
impl<T> Debug for AnimationSet<T> {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		f.debug_struct(std::any::type_name::<Self>()).finish()
	}
}
impl<T> PartialEq for AnimationSet<T> {
	fn eq(&self, _other: &Self) -> bool {
		true
	}
}
impl<T> Eq for AnimationSet<T> {}
impl<T> PartialOrd for AnimationSet<T> {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}
impl<T> Ord for AnimationSet<T> {
	fn cmp(&self, _other: &Self) -> Ordering {
		Ordering::Equal
	}
}
impl<T> Hash for AnimationSet<T> {
	fn hash<H: Hasher>(&self, state: &mut H) {
		// Attempt to avoid collisions by hashing a random constant u128
		98378859208469391100266334942952264424u128.hash(state)
	}
}

pub fn apply_animations<T: Component>(
	mut q: Query<&mut T>,
	mut ticks: ResMut<Events<ComponentDelta<T>>>,
) {
	if ticks.len() == 0 {
		return;
	}
	let mut blendable = HashMap::new();
	for tick in ticks.drain() {
		if tick.progress.is_finite() {
			let (ref mut sum_progress, ref mut blendable_deltas) = blendable
				.entry(tick.target)
				.or_insert_with(|| (0.0, Vec::new()));
			*sum_progress += tick.progress;
			blendable_deltas.push(tick);
		} else {
			let ComponentDelta { target, delta, .. } = tick;
			match q.get_mut(target) {
				Ok(value) => (delta.apply)(value, 1.0),
				Err(e) => error!("{e}"),
			}
		}
	}
	for (_, (sum_progress, blendable_deltas)) in blendable {
		let num = blendable_deltas.len() as f32;
		for tick in blendable_deltas {
			let ComponentDelta {
				target,
				delta: Delta { progress, apply },
			} = tick;
			let coef = if sum_progress > f32::EPSILON {
				progress / sum_progress
			} else {
				1.0 / num
			};
			match q.get_mut(target) {
				Ok(value) => apply(value, coef),
				Err(e) => error!("{e}"),
			}
		}
	}
}

pub fn apply_res_animations<T: Resource>(mut val: ResMut<T>, mut ticks: ResMut<Events<Delta<T>>>) {
	if ticks.len() == 0 {
		return;
	}
	let mut blendable_ticks = Vec::new();
	let mut sum_progress = 0.0;
	for tick in ticks.drain() {
		if tick.progress.is_finite() {
			sum_progress += tick.progress;
			blendable_ticks.push(tick);
		} else {
			(tick.apply)(val.reborrow(), 1.0);
		}
	}
	let num = blendable_ticks.len() as f32;
	for Delta { progress, apply } in blendable_ticks {
		let coef = if sum_progress > f32::EPSILON {
			progress / sum_progress
		} else {
			1.0 / num
		};
		apply(val.reborrow(), coef);
	}
}

#[derive(Event, Deref, DerefMut)]
pub struct ComponentDelta<T: Component> {
	/// The target entity to be animated.
	pub target: Entity,
	#[deref]
	pub delta: Delta<T>,
}

impl<T: Component> ComponentDelta<T> {
	pub fn new(
		target: Entity,
		progress: f32,
		apply: impl FnOnce(Mut<T>, f32) + Send + Sync + 'static,
	) -> Self {
		Self {
			target,
			delta: Delta {
				progress,
				apply: Box::new(apply),
			},
		}
	}

	/// Like `ComponentDelta::diffable` but uses `Mut::map_unchanged` to enable
	/// access to individual fields of a component.
	pub fn mapped<U, F>(target: Entity, progress: f32, new_value: U, f: F) -> Self
	where
		F: FnOnce(&mut T) -> &mut U + Send + Sync + 'static,
		U: Diff + Clone + Add<<U as Diff>::Delta, Output = U> + Send + Sync + 'static,
		<U as Diff>::Delta: Mul<f32, Output = <U as Diff>::Delta> + Default + PartialEq,
	{
		Self::new(target, progress, move |val, coef| {
			let mut val = val.map_unchanged(f);
			let diff = new_value.delta_from(&*val) * coef;
			if diff != <<U as Diff>::Delta as Default>::default() {
				*val = val.clone() + diff;
			}
		})
	}

	/// A delta for an animation with an indefinite progress. `progress` will always be `f32::NAN`,
	/// so the animation will be non-blendable and always run before all blendable animations.
	pub fn indefinite(target: Entity, apply: impl FnOnce(Mut<T>) + Send + Sync + 'static) -> Self {
		Self {
			target,
			delta: Delta {
				progress: f32::NAN,
				apply: Box::new(|val, _| apply(val)),
			},
		}
	}

	/// A delta for a "default" animation. `progress` will always be `0.0`, so the animation will be blended,
	/// but will only contribute any change if there are no running animations with `progress > 0.0`. The
	/// coefficient passed to `apply` will be nonzero if and only if no other animations have made any progress.
	///
	/// Usually there will be only one default animation for a component on an entity, so the coefficient will
	/// be either `0.0` or `1.0`. However, multiple "default" animations technically can be blended together.
	pub fn default_animation(
		target: Entity,
		apply: impl FnOnce(Mut<T>, f32) + Send + Sync + 'static,
	) -> Self {
		Self::new(target, 0.0, apply)
	}

	/// A delta for a component that implements `Diff`.
	pub fn diffable(target: Entity, progress: f32, new_value: T) -> Self
	where
		T: Diff + Clone + Add<<T as Diff>::Delta, Output = T>,
		<T as Diff>::Delta: Mul<f32, Output = <T as Diff>::Delta> + Default + PartialEq,
	{
		Self::new(target, progress, move |mut val, coef| {
			let diff = new_value.delta_from(&*val) * coef;
			if diff != <<T as Diff>::Delta as Default>::default() {
				*val = val.clone() + diff;
			}
		})
	}

	/// See [Self::default_animation] and [Self::diffable].
	pub fn default_diffable(target: Entity, new_value: T) -> Self
	where
		T: Diff + Clone + Add<<T as Diff>::Delta, Output = T>,
		<T as Diff>::Delta: Mul<f32, Output = <T as Diff>::Delta> + Default + PartialEq,
	{
		Self::diffable(target, 0.0, new_value)
	}
}

pub type DynApply<T> = dyn FnOnce(Mut<T>, f32) + Send + Sync + 'static;

pub struct Delta<T> {
	/// Current progress the animation has made. Return a finite number if progress can
	/// be determined and the animation should be blended. Any non-finite number, such
	/// as `f32::NAN`, will cause the animation to be applied with a coefficient of `1.0`.
	/// It would be nice to use something like `Option<FiniteF32>`, but currently Rust
	/// cannot apply the niche optimization to that type, and in this case the non-finite
	/// values represent an "indefinite" animation, thus values like `inifinity` and `NaN`
	/// do what you'd expect in this application.
	pub progress: f32,
	/// Apply this animation tick to the resource. The `f32` parameter is the coefficient
	/// that the delta should be multiplied by, if applicable.
	pub apply: Box<DynApply<T>>,
}

// A `Delta` with no `Entity` is only an `Event` for `Resource`s, not `Component`s.
impl<T: Resource> Event for Delta<T> {}

impl<T> Delta<T> {
	pub fn new(progress: f32, apply: impl FnOnce(Mut<T>, f32) + Send + Sync + 'static) -> Self {
		Self {
			progress,
			apply: Box::new(apply),
		}
	}

	pub fn indefinite(apply: impl FnOnce(Mut<T>) + Send + Sync + 'static) -> Self {
		Self {
			progress: f32::NAN,
			apply: Box::new(|val, _| apply(val)),
		}
	}
}

pub struct AnimationController<'w, 's, 'a> {
	cmds: EntityCommands<'w, 's, 'a>,
}

impl<'w, 's> AnimationController<'w, 's, '_> {
	pub fn end(&mut self) {
		self.cmds.despawn();
	}

	pub fn commands(&mut self) -> &mut Commands<'w, 's> {
		self.cmds.commands()
	}
}

pub type DynAnimateRes<T> =
	dyn FnMut(Res<T>, Res<Time>, AnimationController) -> Delta<T> + Send + Sync + 'static;

#[derive(Component, Deref, DerefMut)]
pub struct DynResAnimation<T: Resource>(pub Box<DynAnimateRes<T>>);

pub type DynAnimateComponent<T> = dyn FnMut(Entity, Ref<T>, Res<Time>, AnimationController) -> ComponentDelta<T>
	+ Send
	+ Sync
	+ 'static;

#[derive(Component)]
pub struct DynAnimation<T: Component>(pub Entity, pub Box<DynAnimateComponent<T>>);

impl<T: Resource> DynResAnimation<T> {
	pub fn animate_res(
		mut cmds: Commands,
		mut animations: Query<(Entity, &mut Self)>,
		target: Res<T>,
		t: Res<Time>,
		mut sender: EventWriter<Delta<T>>,
	) {
		for (id, mut this) in &mut animations {
			sender.send(this(
				Res::clone(&target),
				Res::clone(&t),
				AnimationController {
					cmds: cmds.entity(id),
				},
			))
		}
	}
}

impl<T: Component> DynAnimation<T> {
	pub fn animate(
		mut cmds: Commands,
		mut animations: Query<(Entity, &mut Self)>,
		targets: Query<(Entity, Ref<T>)>,
		t: Res<Time>,
		mut sender: EventWriter<ComponentDelta<T>>,
	) {
		for (id, mut this) in &mut animations {
			let Self(target, ref mut apply) = *this;
			let mut ctrl = AnimationController {
				cmds: if let Some(cmds) = cmds.get_entity(id) {
					cmds
				} else {
					continue;
				},
			};
			match targets.get(target) {
				Ok((id, val)) => sender.send(apply(id, val, Res::clone(&t), ctrl)),
				Err(e) => {
					error!("{e}");
					ctrl.end()
				}
			}
		}
	}
}

#[derive(Component)]
pub struct AnimationHandle<Track>(Entity, PhantomData<Track>);

impl<Track> AnimationHandle<Track> {
	pub fn id(self) -> Entity {
		self.0
	}
}

impl<Track> Clone for AnimationHandle<Track> {
	fn clone(&self) -> Self {
		*self
	}
}

impl<Track> Copy for AnimationHandle<Track> {}

impl<Track> Default for AnimationHandle<Track> {
	fn default() -> Self {
		Self(Entity::PLACEHOLDER, default())
	}
}

impl<Track> Debug for AnimationHandle<Track> {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		f.debug_tuple(std::any::type_name::<Self>())
			.field(&self.0)
			.finish()
	}
}

pub trait StartAnimation {
	fn start_animation<T: Component>(
		&mut self,
		animation: impl FnMut(Entity, Ref<T>, Res<Time>, AnimationController) -> ComponentDelta<T>
			+ Send
			+ Sync
			+ 'static,
	) -> AnimationHandle<DynAnimation<T>>;
}

impl StartAnimation for EntityCommands<'_, '_, '_> {
	fn start_animation<T: Component>(
		&mut self,
		animation: impl FnMut(Entity, Ref<T>, Res<Time>, AnimationController) -> ComponentDelta<T>
			+ Send
			+ Sync
			+ 'static,
	) -> AnimationHandle<DynAnimation<T>> {
		let id = self.id();
		let mut cmds = self.commands().spawn(DynAnimation(id, Box::new(animation)));
		let handle = AnimationHandle(cmds.id(), default());
		cmds.insert(handle);
		handle
	}
}

pub trait StartResAnimation {
	fn start_res_animation<T: Resource>(
		&mut self,
		animation: impl FnMut(Res<T>, Res<Time>, AnimationController) -> Delta<T>
			+ Send
			+ Sync
			+ 'static,
	) -> AnimationHandle<DynResAnimation<T>>;
}

impl StartResAnimation for Commands<'_, '_> {
	fn start_res_animation<T: Resource>(
		&mut self,
		animation: impl FnMut(Res<T>, Res<Time>, AnimationController) -> Delta<T>
			+ Send
			+ Sync
			+ 'static,
	) -> AnimationHandle<DynResAnimation<T>> {
		let mut cmds = self.spawn(DynResAnimation(Box::new(animation)));
		let handle = AnimationHandle(cmds.id(), default());
		cmds.insert(handle);
		handle
	}
}

#[derive(Component, Debug)]
pub struct BlendTargets {
	pub animated: Entity,
	pub to: Target,
	pub from: Target,
	pub duration: Duration,
	pub elapsed: Duration,
	pub easing: Option<fn(f32) -> f32>,
}

impl BlendTargets {
	pub fn new(animated: Entity, from: Target, to: Target, duration: Duration) -> Self {
		Self {
			animated,
			to,
			from,
			duration,
			elapsed: Duration::ZERO,
			easing: None,
		}
	}

	pub fn with_easing(self, easing: fn(f32) -> f32) -> Self {
		Self {
			easing: Some(easing),
			..self
		}
	}

	pub fn animate(
		mut cmds: Commands,
		global_xforms: Query<&GlobalTransform>,
		parents: Query<&Parent>,
		mut animations: Query<(Entity, &mut Self)>,
		mut sender: EventWriter<ComponentDelta<Transform>>,
		t: Res<Time>,
	) {
		for (id, mut state) in &mut animations {
			state.elapsed += t.delta();
			if state.elapsed >= state.duration {
				state.elapsed = state.duration;
				cmds.entity(id).despawn();
			}

			let to_global = match state.to.global(state.animated, &global_xforms) {
				Ok(global) => global,
				Err(e) => {
					error!("{e}");
					cmds.entity(id).despawn();
					continue;
				}
			}
			.compute_transform();

			let from_global = match state.from.global(state.animated, &global_xforms) {
				Ok(global) => global,
				Err(e) => {
					error!("{e}");
					cmds.entity(id).despawn();
					continue;
				}
			}
			.compute_transform();

			let progress = state.elapsed.as_secs_f32() / state.duration.as_secs_f32();
			let progress = if let Some(easing) = state.easing {
				easing(progress)
			} else {
				progress
			};
			let new_global = from_global + (to_global.delta_from(&from_global) * progress);
			let new = if let Ok(parent) = parents.get(state.animated) {
				match global_xforms.get(parent.get()) {
					Ok(parent_global) => {
						GlobalTransform::from(new_global).reparented_to(parent_global)
					}
					Err(QueryEntityError::QueryDoesNotMatch(_)) => new_global, // Parent has no GlobalTransform
					Err(e) => {
						error!("Can't find parent: {e}");
						new_global
					}
				}
			} else {
				new_global
			};
			sender.send(ComponentDelta::<Transform>::diffable(
				state.animated,
				progress,
				new,
			))
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn animation_api() {
		let _ = App::new()
			.add_plugins(BuiltinAnimations)
			.add_systems(Update, Slide::tick);

		#[derive(Component, Copy, Clone)]
		pub struct Slide(Entity, Vec3);

		impl Slide {
			pub fn tick(
				q: Query<(), With<Transform>>,
				animations: Query<&Slide>,
				mut sender: EventWriter<ComponentDelta<Transform>>,
				t: Res<Time>,
			) {
				let dt = t.delta_seconds();
				for Slide(target, vel) in animations.iter().copied() {
					match q.get(target) {
						Ok(_) => sender.send(ComponentDelta::<Transform>::indefinite(
							target,
							move |mut xform| xform.translation += vel * dt,
						)),
						Err(e) => error!("{e}"),
					}
				}
			}
		}
	}
}
