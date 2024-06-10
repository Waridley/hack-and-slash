use super::widgets::WidgetShape;
use crate::util::{LogComponentNames, DEBUG_COMPONENTS};
use bevy::{ecs::query::QueryEntityError, prelude::*};
use rapier3d::parry::query::Ray;
use serde::{Deserialize, Serialize};

#[derive(Component, Debug, Reflect, Serialize, Deserialize)]
#[reflect(Component, Serialize, Deserialize)]
pub struct LineUpChildren {
	/// A unit vector indicates shapes will be touching. Any length over `1.0`
	/// results in separating the shapes by that distance.
	pub relative_positions: Vec3,
	/// Alignment of children. For each axis:
	/// - `0.0` centers children at this entity's origin.
	/// - `-1.0` is analogous to `flex_start`, and `1.0` is analogous to `flex_end`.
	pub align: Vec3,
}

impl Default for LineUpChildren {
	fn default() -> Self {
		Self::horizontal()
	}
}

impl LineUpChildren {
	pub fn horizontal() -> Self {
		Self {
			relative_positions: Vec3::X,
			align: Vec3::ZERO,
		}
	}

	pub fn vertical() -> Self {
		Self {
			relative_positions: Vec3::NEG_Z,
			align: Vec3::ZERO,
		}
	}

	pub fn with_spacing(self, spacing: f32) -> Self {
		Self {
			relative_positions: self.relative_positions.normalize() * (1.0 + spacing),
			align: self.align,
		}
	}

	pub fn with_alignment(self, align: Vec3) -> Self {
		Self {
			relative_positions: self.relative_positions,
			align,
		}
	}
}

#[derive(Component, Debug, Reflect, Serialize, Deserialize)]
#[reflect(Component, Serialize, Deserialize)]
pub struct SiblingConstraint {
	pub a: Entity,
	pub b: Entity,
	/// A unit vector indicates shapes will be touching. Any length over `1.0`
	/// results in separating the shapes by that distance.
	pub relative_pos: Vec3,
}

impl Default for SiblingConstraint {
	fn default() -> Self {
		Self {
			a: Entity::PLACEHOLDER,
			b: Entity::PLACEHOLDER,
			relative_pos: Vec3::X,
		}
	}
}

pub fn apply_constraints(
	mut cmds: Commands,
	child_constraints: Query<(Entity, &LineUpChildren, &Children)>,
	sibling_constraints: Query<&SiblingConstraint>,
	mut transforms: Query<(Entity, &mut Transform, Option<&Parent>, &WidgetShape)>,
) {
	for (id, constraint, children) in &child_constraints {
		match children.len() {
			0 => continue,
			1 => {
				match transforms.get_mut(children[0]) {
					Ok(mut child) => child.1.translation = Vec3::ZERO,
					Err(e) => cmds.debug_components(children[0], e),
				}
				continue;
			}
			_ => {}
		}
		let mut separations = Vec::with_capacity(children.len());
		for pair in children.windows(2) {
			let [a, b] = match transforms.get_many([pair[0], pair[1]]) {
				Ok(pair) => pair,
				Err(e) => {
					cmds.debug_components(pair[0], e);
					cmds.debug_components(pair[1], e);
					continue;
				}
			};
			debug_assert_eq!(Some(id), a.2.map(Parent::get));
			debug_assert_eq!(Some(id), b.2.map(Parent::get));

			let sep = compute_separation(
				constraint.relative_positions,
				a.1.rotation,
				a.3,
				b.1.rotation,
				b.3,
			);
			separations.push(sep);
		}
		let sep_sum = separations.iter().sum::<Vec3>();
		let offset = sep_sum * ((-constraint.align + Vec3::ONE) * 0.5);
		let mut first_child = match transforms.get_mut(children[0]) {
			Ok(child) => child,
			Err(e) => {
				cmds.debug_components(children[0], e);
				continue;
			}
		};
		first_child.1.translation = -offset;
		for (i, pair) in children.windows(2).enumerate() {
			let [a, mut b] = match transforms.get_many_mut([pair[0], pair[1]]) {
				Ok(pair) => pair,
				Err(e) => {
					cmds.debug_components(pair[0], e);
					cmds.debug_components(pair[1], e);
					continue;
				}
			};
			b.1.translation = a.1.translation + separations[i];
		}
	}

	// Let siblings move after they are arranged relative to their parent
	for constraint in &sibling_constraints {
		let [a, mut b] = match transforms.get_many_mut([constraint.a, constraint.b]) {
			Ok(pair) => pair,
			Err(e) => {
				debug!("{e}");
				continue;
			}
		};

		if !(a.2 == b.2) {
			error!(a.parent = ?a.2, b.parent = ?b.2, "{:?} and {:?} do not have the same parent", a.0, b.0);
			continue;
		}

		let sep = compute_separation(
			constraint.relative_pos,
			a.1.rotation,
			a.3,
			b.1.rotation,
			b.3,
		);
		if b.1.translation != a.1.translation + sep {
			b.1.translation = a.1.translation + sep;
		}
	}
}

fn compute_separation(
	desired_relative: Vec3,
	a_rot: Quat,
	a_shape: &WidgetShape,
	b_rot: Quat,
	b_shape: &WidgetShape,
) -> Vec3 {
	let a_bs = a_shape.compute_local_bounding_sphere();
	let b_bs = b_shape.compute_local_bounding_sphere();
	let dir = desired_relative.normalize_or_zero();
	let a_len = a_bs.radius;
	let b_len = b_bs.radius;
	let a_to_b = a_rot * (dir * a_len);
	let b_to_a = b_rot * (-dir * b_len);
	let a_ray = Ray::new(a_to_b.into(), b_to_a.normalize_or_zero().into());
	let b_ray = Ray::new(b_to_a.into(), a_to_b.normalize_or_zero().into());
	let ta = a_shape.cast_local_ray(&a_ray, a_len, true);
	let tb = b_shape.cast_local_ray(&b_ray, b_len, true);
	let (Some(ta), Some(tb)) = (ta, tb) else {
		let msg = "Unexpectedly unable to find contact between shapes";
		if cfg!(debug_assertions) {
			// Catch bad values in development
			panic!(
				"{msg}:\n
			\ta_bs={a_bs:?}, a_to_b={a_to_b:?}, ta={ta:?}\n
			\tb_bs={b_bs:?}, b_to_a={b_to_a:?}, tb={tb:?}"
			);
		} else {
			// Don't crash in production
			error!(?a_bs, ?a_to_b, ?ta, ?b_bs, ?b_to_a, ?tb, "{msg}");
			return Vec3::ZERO;
		}
	};

	let rel = desired_relative;
	let pa = (a_len - ta) * dir;
	let pb = (b_len - tb) * dir;
	let space = rel - rel.normalize_or_zero();
	pa + space + pb
}
