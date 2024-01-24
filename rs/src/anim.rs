use crate::util::Lerp;
use bevy::{
	ecs::{
		query::{QueryItem, WorldQuery},
		system::EntityCommands,
	},
	prelude::*,
	utils::HashMap,
};
use enum_components::bevy_ecs::system::SystemParam;
use std::{
	iter::Sum,
	ops::{Add, Mul, Sub},
};

pub trait CurveMut<T: Component> {
	fn into_mut_tick_fn(
		self,
	) -> impl FnMut(QueryItem<&'static mut T>, Res<Time>) + Send + Sync + 'static;
}

pub trait BlendableCurve<T: Component, Delta: 'static = T> {
	fn into_tick_fn(
		self,
	) -> impl FnMut(QueryItem<Ref<T>>, Res<Time>) -> Delta + Send + Sync + 'static;
}

#[derive(Component, Resource, Debug)]
pub struct LerpCurve<Lhs, Rhs = Lhs> {
	pub start: Lhs,
	pub end: Rhs,
}

impl<Lhs, Rhs> LerpCurve<Lhs, Rhs>
where
	Lhs: Lerp<Rhs, f32> + Clone + PartialEq<Lhs> + Send + Sync + 'static,
	Rhs: Sub<Lhs> + Clone + Send + Sync + 'static,
	<Rhs as Sub<Lhs>>::Output: Mul<f32>,
	<<Rhs as Sub<Lhs>>::Output as Mul<f32>>::Output: Add<Lhs>,
	<<<Rhs as Sub<Lhs>>::Output as Mul<f32>>::Output as Add<Lhs>>::Output:
		Component + Clone + PartialEq,
{
	fn animate_component(
		mut q: Query<(
			&mut Self,
			&mut <<<Rhs as Sub<Lhs>>::Output as Mul<f32>>::Output as Add<Lhs>>::Output,
		)>,
		t: Res<Time>,
	) {
		for (this, mut value) in &mut q {
			let new = this.start.clone().lerp(this.end.clone(), t.delta_seconds());
			if *value != new {
				*value = new;
			}
		}
	}
}

#[derive(Component, Default)]
pub struct LerpSlerpCurve {
	pub start: Transform,
	pub end: Transform,
	pub curr_t: f32,
}

impl CurveMut<Transform> for LerpSlerpCurve {
	fn into_mut_tick_fn(
		mut self,
	) -> impl FnMut(QueryItem<&'static mut Transform>, Res<Time>) + Send + Sync + 'static {
		move |mut value, t: Res<Time>| {
			let t = t.delta_seconds();
			self.curr_t += t;
			let new = Transform {
				translation: self.start.translation.lerp(self.end.translation, t),
				rotation: self.start.rotation.slerp(self.end.rotation, t),
				scale: self.start.scale.lerp(self.end.scale, t),
			};
			if *value != new {
				*value = new;
			}
		}
	}
}

impl BlendableCurve<Transform, TransformDelta> for LerpSlerpCurve {
	fn into_tick_fn(
		mut self,
	) -> impl FnMut(QueryItem<Ref<Transform>>, Res<Time>) -> TransformDelta + Send + Sync + 'static
	{
		move |_, t: Res<Time>| {
			let t = t.delta_seconds();
			self.curr_t += t;
			TransformDelta(Transform {
				translation: (self.end.translation - self.start.translation) * t,
				rotation: Quat::IDENTITY
					.slerp(self.start.rotation.inverse() * self.end.rotation, t),
				scale: (self.end.scale - self.start.scale) * t,
			})
		}
	}
}

#[derive(Component)]
pub struct MutAnimation<T: Component> {
	pub entity: Entity,
	pub tick: Box<dyn FnMut(QueryItem<&'static mut T>, Res<Time>) + Send + Sync + 'static>,
}

impl<T: Component + 'static> MutAnimation<T> {
	pub fn new(
		entity: Entity,
		tick: impl FnMut(QueryItem<&'static mut T>, Res<Time>) + Send + Sync + 'static,
	) -> Self {
		Self {
			entity,
			tick: Box::new(tick),
		}
	}

	/// System for running all mutating animations for a given component on a given entity.
	///
	/// # WARNING
	/// There is no guarantee in what order animations will be applied. Usually, you will want
	/// to manually make sure only one animation is actually spawned at a given time.
	pub fn animate(mut this_q: Query<&mut Self>, mut target_q: Query<&mut T>, time: Res<Time>) {
		for mut this in &mut this_q {
			let Self { entity, tick } = &mut *this;
			match target_q.get_mut(*entity) {
				Ok(target) => tick(target, Res::clone(&time)),
				Err(e) => bevy::log::error!("{e}"),
			}
		}
	}
}

#[derive(Component)]
pub struct BlendableAnimation<T: Component, Delta> {
	pub entity: Entity,
	pub tick:
		Box<dyn FnMut(QueryItem<Ref<'static, T>>, Res<Time>) -> Delta + Send + Sync + 'static>,
}

impl<
		T: Component + Add<Delta, Output = T> + PartialEq + Clone,
		Delta: Mul<f32, Output = Delta> + 'static,
	> BlendableAnimation<T, Delta>
{
	pub fn new(
		entity: Entity,
		tick: impl FnMut(QueryItem<Ref<'static, T>>, Res<Time>) -> Delta + Send + Sync + 'static,
	) -> Self {
		Self {
			entity,
			tick: Box::new(tick),
		}
	}

	/// System for blending all animations for a given component for a given entity.
	pub fn animate(mut this_q: Query<&mut Self>, mut target_q: Query<&mut T>, time: Res<Time>) {
		let mut to_blend = HashMap::<Entity, Vec<Delta>>::new();

		for mut this in &mut this_q {
			let Self { entity, tick } = &mut *this;
			match target_q.get_mut(*entity) {
				Ok(target) => {
					let mut to_blend = to_blend
						.entry(*entity)
						.or_insert_with(|| Vec::with_capacity(1));
					to_blend.push(tick(target.into(), Res::clone(&time)))
				}
				Err(e) => bevy::log::error!("{e}"),
			}
		}

		for (id, mut to_blend) in to_blend.drain() {
			let coef = 1.0 / to_blend.len() as f32;
			let mut val = target_q.get_mut(id).unwrap();
			let new = to_blend
				.drain(..)
				.map(|delta| delta * coef)
				.fold(val.clone(), |acc, item| acc + item);
			if *val != new {
				*val = new;
			}
		}
	}
}

pub trait StartAnimation<'w, 's, 'a> {
	fn start_mut_animation<T: Component>(
		&'a mut self,
		tick: impl FnMut(QueryItem<&'static mut T>, Res<Time>) + Send + Sync + 'static,
	) -> EntityCommands<'w, 's, 'a>;
	fn start_blendable_animation<
		T: Component + Add<Delta, Output = T> + PartialEq + Clone,
		Delta: Mul<f32, Output = Delta> + 'static,
	>(
		&'a mut self,
		tick: impl FnMut(QueryItem<Ref<'static, T>>, Res<Time>) -> Delta + Send + Sync + 'static,
	) -> EntityCommands<'w, 's, 'a>;
}

impl<'w, 's, 'a> StartAnimation<'w, 's, 'a> for EntityCommands<'w, 's, 'a> {
	fn start_mut_animation<T: Component>(
		&'a mut self,
		tick: impl FnMut(QueryItem<&'static mut T>, Res<Time>) + Send + Sync + 'static,
	) -> EntityCommands<'w, 's, 'a> {
		let id = self.id();
		self.commands().spawn(MutAnimation::new(id, tick))
	}

	fn start_blendable_animation<
		T: Component + Add<Delta, Output = T> + PartialEq + Clone,
		Delta: Mul<f32, Output = Delta> + 'static,
	>(
		&'a mut self,
		tick: impl FnMut(QueryItem<Ref<'static, T>>, Res<Time>) -> Delta + Send + Sync + 'static,
	) -> EntityCommands<'w, 's, 'a> {
		let id = self.id();
		self.commands().spawn(BlendableAnimation::new(id, tick))
	}
}

pub struct TransformDelta(Transform);

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
			rotation: Quat::default().slerp(self.0.rotation, rhs),
			scale: self.0.scale * rhs,
		})
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use bevy::prelude::*;

	#[test]
	fn animation_api() {
		let _ = App::new().add_systems(
			Update,
			(
				MutAnimation::<Transform>::animate,
				BlendableAnimation::<Transform, TransformDelta>::animate,
			),
		);

		fn spawn_animations(mut cmds: Commands, mut q: Query<(Entity, &Transform)>) {
			for (id, xform) in &mut q {
				cmds.entity(id).start_blendable_animation(
					LerpSlerpCurve {
						start: *xform,
						end: Transform {
							translation: Vec3::ONE,
							..default()
						},
						..default()
					}
					.into_tick_fn(),
				);
			}
		}
	}
}
