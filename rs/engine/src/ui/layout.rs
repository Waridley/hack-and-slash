use super::widgets::WidgetShape;
use bevy::ecs::query::QueryEntityError;
use bevy::prelude::*;

#[derive(Component, Debug)]
pub struct ChildrenConstraint {
	pub relative_positions: Vec2,
}

#[derive(Component, Debug)]
pub struct SiblingConstraint {
	pub a: Entity,
	pub b: Entity,
	pub relative_pos: Vec2,
}

pub fn apply_constraints(
	child_constraints: Query<(Entity, &ChildrenConstraint, &Children)>,
	sibling_constraints: Query<&SiblingConstraint>,
	mut transforms: Query<(Entity, &mut Transform, Option<&Parent>, &WidgetShape)>,
) {
	for (id, constraint, children) in &child_constraints {
		for pair in children.windows(2) {
			let [a, b] = match transforms.get_many_mut([pair[0], pair[1]]) {
				Ok(pair) => pair,
				Err(e) => {
					error!("{e}");
					continue;
				}
			};
			debug_assert_eq!(Some(id), a.2.map(Parent::get));
			debug_assert_eq!(Some(id), b.2.map(Parent::get));

			let ((a, a_shape), (mut b, b_shape)) = ((a.1, a.3), (b.1, b.3));
			let rel = constraint.relative_positions.into();
			let pa = Vec2::from(a_shape.project_local_point(&rel, false).point);
			let pb = Vec2::from(b_shape.project_local_point(&-rel, false).point);
			let rel = constraint.relative_positions;
			let new = a.translation.xz() + pa + (rel - rel.normalize_or_zero()) + -pb;
			if b.translation.xz() != new {
				b.translation.x = new.x;
				b.translation.z = new.y;
			}
		}
	}

	for constraint in &sibling_constraints {
		let [a, b] = match transforms.get_many_mut([constraint.a, constraint.b]) {
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

		let ((a, a_shape), (mut b, b_shape)) = ((a.1, a.3), (b.1, b.3));
		let rel = constraint.relative_pos.into();
		let pa = Vec2::from(a_shape.project_local_point(&rel, false).point);
		let pb = Vec2::from(b_shape.project_local_point(&-rel, false).point);
		let rel = constraint.relative_pos;
		let new = a.translation.xz() + pa + (rel - rel.normalize_or_zero()) + pb;
		if b.translation.xz() != new {
			b.translation.x = new.x;
			b.translation.z = new.y;
		}
	}
}
