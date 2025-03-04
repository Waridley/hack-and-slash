use super::widgets::{CuboidContainer, CuboidPanel, CylinderPanel, WidgetShape};
use crate::util::Angle;
use bevy::prelude::*;
use bevy_rapier3d::{
	parry::{
		bounding_volume::{Aabb, BoundingVolume},
		math::{Isometry, Translation, Vector},
		query::cast_shapes,
	},
	prelude::ShapeCastOptions,
};
use itertools::Itertools;
use rapier3d::na::Vector3;
use serde::{Deserialize, Serialize};
use std::f32::consts::PI;

#[derive(Component, Clone, Copy, Debug, Reflect, Serialize, Deserialize)]
#[reflect(Component, Serialize, Deserialize)]
pub struct LineUpChildren {
	/// A unit vector indicates shapes will be touching. Any length over `1.0`
	/// results in separating the shapes by that distance.
	pub relative_positions: Vec3,
	/// Alignment of children proportional to `relative_positions`. For each axis:
	/// - `0.0` centers children at this entity's origin.
	/// - `-1.0` is analogous to `flex_start`, and `1.0` is analogous to `flex_end`.
	///
	/// NOTE: This has no effect for axes with `0.0` relative position.
	pub align: Vec3,
	/// Translates the origin point of alignment.
	pub offset: Vec3,
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
			offset: Vec3::ZERO,
		}
	}

	pub fn vertical() -> Self {
		Self {
			relative_positions: Vec3::NEG_Z,
			align: Vec3::ZERO,
			offset: Vec3::ZERO,
		}
	}

	pub fn with_spacing(self, spacing: f32) -> Self {
		Self {
			relative_positions: self.relative_positions.normalize() * (1.0 + spacing),
			align: self.align,
			offset: self.offset,
		}
	}

	pub fn with_alignment(self, align: Vec3) -> Self {
		Self {
			relative_positions: self.relative_positions,
			align,
			offset: self.offset,
		}
	}

	pub fn with_offset(self, offset: Vec3) -> Self {
		Self {
			relative_positions: self.relative_positions,
			align: self.align,
			offset,
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
	child_constraints: Query<(Entity, &LineUpChildren, &Children)>,
	sibling_constraints: Query<&SiblingConstraint>,
	mut shapes: Query<(Entity, &mut Transform, Option<&Parent>, &WidgetShape)>,
) {
	for (container, constraint, children) in &child_constraints {
		match children.len() {
			0 => continue,
			1 => {
				match shapes.get_mut(children[0]) {
					Ok(mut child) => {
						if child.1.translation != constraint.offset {
							child.1.translation = constraint.offset
						}
					}
					Err(e) => trace!(?container, "Get only child for translation: {e}"),
				}
				continue;
			}
			_ => {}
		}
		let mut separations = Vec::with_capacity(children.len());
		for pair in children
			.iter()
			.copied()
			.filter(|child| {
				shapes
					.get(*child)
					.inspect_err(|e| trace!(?container, "Get children for separation: {e}"))
					.is_ok()
			})
			.tuple_windows::<(_, _)>()
		{
			let [a, b] = shapes
				.get_many([pair.0, pair.1])
				.expect("pairs are filtered for entities that already match `shapes`");

			let sep = compute_separation(constraint.relative_positions, a.3, b.3);
			separations.push((pair, sep));
		}
		let sep_sum = separations.iter().map(|(_, sep)| sep).sum::<Vec3>();
		let offset = sep_sum * ((-constraint.align + Vec3::ONE) * 0.5);
		let Some(&((first_child, _), _)) = separations.get(0) else {
			continue;
		};
		let mut first_child = shapes
			.get_mut(first_child)
			.expect("already got this entity when computing separation");
		first_child.1.translation = -offset + constraint.offset;
		for ((a, b), sep) in separations {
			let [a, mut b] = shapes
				.get_many_mut([a, b])
				.expect("already got these entities when computing separation");
			b.1.translation = a.1.translation + sep;
		}
	}

	// Let siblings move after they are arranged relative to their parent
	for constraint in &sibling_constraints {
		let [a, mut b] = match shapes.get_many_mut([constraint.a, constraint.b]) {
			Ok(pair) => pair,
			Err(e) => {
				debug!(?constraint, "Get pair for sibling constraints: {e}");
				continue;
			}
		};

		if !(a.2 == b.2) {
			error!(a.parent = ?a.2, b.parent = ?b.2, "{:?} and {:?} do not have the same parent", a.0, b.0);
			continue;
		}

		let sep = compute_separation(constraint.relative_pos, a.3, b.3);
		if b.1.translation != a.1.translation + sep {
			b.1.translation = a.1.translation + sep;
		}
	}
}

fn compute_separation(desired_relative: Vec3, a: &WidgetShape, b: &WidgetShape) -> Vec3 {
	let a_pos = a.isometry;
	let b_pos = b.isometry;
	// Get the bounding spheres to know how far to cast the rays
	let a_bs = a.compute_bounding_sphere(&a_pos);
	let b_bs = b.compute_bounding_sphere(&b_pos);
	let dir = Vector::from(desired_relative.normalize_or_zero());
	let a_len = a_bs.radius;
	let b_len = b_bs.radius;
	let iso_rel = b.isometry.translation.vector - a.isometry.translation.vector;
	let start_dist = a_len + b_len + Vec3::from(iso_rel).length();
	let b_start = dir * start_dist;
	let Ok(toi) = cast_shapes(
		&a.isometry,
		&Vector::zeros(),
		&*a.shape.0,
		&Isometry {
			translation: b.isometry.translation * Translation::from(b_start),
			rotation: b.isometry.rotation,
		},
		&-dir,
		&*b.shape.0,
		ShapeCastOptions {
			max_time_of_impact: start_dist,
			..default()
		},
	) else {
		if cfg!(debug_assertions) {
			panic!("ToI between {a:?} and {b:?} unsupported");
		} else {
			error!("ToI between {a:?} and {b:?} unsupported");
			return Vec3::ZERO;
		}
	};

	let Some(hit) = toi else {
		warn!(?a, ?b, "Unable to find contact between shapes");
		return Vec3::ZERO;
	};
	let dir = Vec3::from(dir);
	((start_dist - hit.time_of_impact) * dir) + (desired_relative - dir)
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

#[derive(Component, Default, Clone, Debug, Reflect, Serialize, Deserialize)]
#[reflect(Component, Serialize, Deserialize)]
pub struct ExpandToFitChildren {
	pub margin: Vec3,
	pub offset: Vec3,
	pub min_size: Vec3,
}

impl ExpandToFitChildren {
	pub fn apply<Container: Component + SetSize>(
		mut q: Query<(&Self, &mut Container, &Children)>,
		children: Query<(Ref<WidgetShape>, Ref<Transform>)>,
	) {
		for (this, desc, kids) in &mut q {
			let mut aabb = Aabb::new_invalid();
			if !kids.iter().copied().any(|child| {
				children.get(child).map_or(false, |(shape, xform)| {
					shape.is_changed() || xform.is_changed()
				})
			}) {
				continue;
			}
			for child in kids.iter().copied() {
				let (shape, xform) = match children.get(child) {
					Ok(child) => child,
					Err(e) => {
						error!("Couldn't get child from `children` query: {e}");
						continue;
					}
				};
				aabb.merge(&shape.compute_aabb(
					&(Isometry::new(
						xform.translation.into(),
						xform.rotation.to_scaled_axis().into(),
					) * shape.isometry),
				));
			}
			let size = aabb.extents();
			let undersize = (this.min_size - Vec3::from(size)).max(Vec3::ZERO);
			let extra = Vector3::from(undersize * 0.5);
			aabb.mins -= extra;
			aabb.maxs += extra;
			aabb.mins -= Vector3::from(this.margin);
			aabb.maxs += Vector3::from(this.margin);
			aabb.mins += Vector3::from(this.offset);
			aabb.maxs += Vector3::from(this.offset);
			SetSize::set_size(desc, aabb);
		}
	}
}

pub trait SetSize {
	fn set_size(this: Mut<Self>, aabb: Aabb);
}

impl SetSize for CuboidPanel {
	fn set_size(mut this: Mut<Self>, aabb: Aabb) {
		let size = aabb.extents().into();
		let center = aabb.center().into();
		if this.size != size {
			this.size = size;
		}
		if this.vertex_translation != center {
			this.vertex_translation = center;
		}
	}
}

impl SetSize for CylinderPanel {
	fn set_size(mut this: Mut<Self>, aabb: Aabb) {
		let extents = aabb.extents();
		let inradius = Vec2::from(extents.xz()).length();
		let length = extents.z;
		let radius = inradius / (PI / this.subdivisions as f32).cos();
		if this.radius != radius {
			this.radius = radius;
		}
		if this.length != length {
			this.length = length;
		}
	}
}

impl SetSize for CuboidContainer {
	fn set_size(mut this: Mut<Self>, aabb: Aabb) {
		let size = aabb.extents().into();
		let center = aabb.center().into();
		if this.size != size {
			this.size = size;
		}
		if this.translation != center {
			this.translation = center;
		}
	}
}
