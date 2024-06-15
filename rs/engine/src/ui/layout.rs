use super::widgets::WidgetShape;
use crate::util::{Angle, LogComponentNames, DEBUG_COMPONENTS};
use bevy::{ecs::query::QueryEntityError, prelude::*};
use rapier3d::parry::query::Ray;
use serde::{Deserialize, Serialize};
use std::f32::consts::TAU;

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

#[derive(Component, Clone, Debug, Reflect, Serialize, Deserialize)]
#[reflect(Component, Serialize, Deserialize)]
pub struct RadialChildren {
	pub radius: f32,
	pub orientation: Vec3,
	pub arrangement: RadialArrangement,
}

impl Default for RadialChildren {
	fn default() -> Self {
		Self {
			radius: 4.0,
			orientation: Vec3::NEG_Y,
			arrangement: default(),
		}
	}
}

impl RadialChildren {
	pub fn apply(
		q: Query<(Entity, &Self, &Children), Changed<Self>>,
		mut xforms: Query<(&mut Transform, &Parent)>,
	) {
		for (id, this, children) in &q {
			let first = this.arrangement.first();
			let sep = this.arrangement.separation(children.len());
			for i in 0..children.len() {
				let angle = first.rad() + (sep.rad() * i as f32);
				let dir = Vec2::from_angle(angle);
				let dir = Vec3::new(dir.x, 0.0, dir.y);
				let rot = Quat::from_rotation_arc(Vec3::NEG_Y, this.orientation);
				let dir = rot * dir;
				let (mut xform, _parent) = match xforms.get_mut(children[i]) {
					Ok((xform, parent)) => (xform, parent),
					Err(e) => {
						error!(?id, ?this, i, "{e}");
						continue;
					}
				};
				debug_assert_eq!(id, _parent.get());
				let new_pos = dir * this.radius;
				if xform.translation != new_pos {
					xform.translation = new_pos;
				}
				if xform.rotation != rot {
					xform.rotation = rot;
				}
			}
		}
	}
}

#[derive(Copy, Clone, Debug, Default, Reflect, Serialize, Deserialize)]
#[reflect(Serialize, Deserialize)]
pub enum RadialArrangement {
	/// Arranges children evenly dividing the circle.
	#[default]
	Auto,
	/// Manually set the separation angle between children.
	Manual { separation: Angle, first: Angle },
}

impl RadialArrangement {
	pub const fn first(self) -> Angle {
		match self {
			RadialArrangement::Auto => Angle::ZERO,
			RadialArrangement::Manual { first, .. } => first,
		}
	}

	pub const fn separation(self, num_children: usize) -> Angle {
		match self {
			RadialArrangement::Auto => Angle::TauOver(num_children as f32),
			RadialArrangement::Manual { separation, .. } => separation,
		}
	}
}
