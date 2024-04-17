use super::widgets::WidgetShape;
use bevy::prelude::*;
use num_traits::Zero;
use rapier3d::math::Point;
use rapier3d::parry::query::Ray;
use rapier3d::{math::Isometry, na::Vector3, parry::query::Unsupported};
use serde::{Deserialize, Serialize};

#[derive(Component, Debug, Reflect, Serialize, Deserialize)]
#[reflect(Component, Serialize, Deserialize)]
pub struct ChildrenConstraint {
	/// A unit vector indicates shapes will be touching. Any length over `1.0`
	/// results in separating the shapes by that distance.
	pub relative_positions: Vec3,
	/// Alignment of children along the `relative_positions` axis.
	/// `0.0` centers children at this entity's origin.
	/// `-1.0` is analogous to `flex_start`, and `1.0` is analogous to `flex_end`.
	pub alignment: f32,
}

impl Default for ChildrenConstraint {
	fn default() -> Self {
		Self {
			relative_positions: Vec3::X,
			alignment: 1.0,
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
	child_constraints: Query<(Entity, &ChildrenConstraint, &Children)>,
	sibling_constraints: Query<&SiblingConstraint>,
	mut transforms: Query<(Entity, &mut Transform, Option<&Parent>, &WidgetShape)>,
) {
	for (id, constraint, children) in &child_constraints {
		if children.len() < 2 {
			continue;
		}
		let mut separations = Vec::with_capacity(children.len());
		for pair in children.windows(2) {
			let [a, b] = match transforms.get_many([pair[0], pair[1]]) {
				Ok(pair) => pair,
				Err(e) => {
					error!("{e}");
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
		let offset = sep_sum * ((-constraint.alignment + 1.0) * 0.5);
		transforms.get_mut(children[0]).unwrap().1.translation = -offset;
		for (i, pair) in children.windows(2).enumerate() {
			let [a, mut b] = transforms.get_many_mut([pair[0], pair[1]]).unwrap();
			b.1.translation = a.1.translation + separations[i];
		}
	}

	// Let siblings move after they are arranged relative to their parent
	for constraint in &sibling_constraints {
		let [a, mut b] = match transforms.get_many_mut([constraint.a, constraint.b]) {
			Ok(pair) => pair,
			Err(e) => {
				error!("{e}");
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
	let dir = desired_relative.normalize();
	let a_len = a_bs.radius * 1.1;
	let b_len = b_bs.radius * 1.1;
	let a_ray = Ray::new((a_rot * -dir * a_len).into(), dir.into());
	let b_ray = Ray::new((b_rot * dir * b_len).into(), (-dir).into());
	let ta = a_shape.cast_local_ray(&a_ray, a_len, true);
	let tb = b_shape.cast_local_ray(&b_ray, b_len, true);
	let (Some(ta), Some(tb)) = (ta, tb) else {
		let msg = "Unexpectedly unable to find contact between shapes";
		if cfg!(debug_assertions) {
			panic!("{msg}");
		} else {
			// Don't crash in production
			error!("{msg}");
			return Vec3::ZERO;
		}
	};

	let rel = desired_relative;
	let pa = (a_len - ta) * dir;
	let pb = (b_len - tb) * dir;
	let space = rel - rel.normalize_or_zero();
	pa + space + pb
}
