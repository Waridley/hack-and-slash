use bevy::{ecs::system::SystemParam, prelude::*};
use bevy_rapier3d::{parry::math::Vector, prelude::*};
use rapier3d::prelude::ShapeType::HeightField;

#[derive(SystemParam)]
pub struct OneWayHeightFieldFilter<'w, 's> {
	colliders: Query<'w, 's, &'static Collider>,
}

impl OneWayHeightFieldFilter<'_, '_> {
	pub fn should_flip_normal(
		&self,
		collider: Entity,
		local_norm: &Vector<f32>,
		up: &Vector<f32>,
	) -> bool {
		if self.colliders.get(collider).map(|col| col.raw.shape_type()) == Ok(HeightField) {
			if local_norm.dot(up) < 0.0 {
				return true;
			}
		}
		false
	}

	pub fn filter(
		&self,
		collider1: Entity,
		collider2: Entity,
		local_n1: &Vector<f32>,
		local_n2: &Vector<f32>,
		up: &Vector<f32>,
		normal: &mut Vector<f32>,
	) {
		if self.should_flip_normal(collider1, local_n1, up)
			|| self.should_flip_normal(collider2, local_n2, up)
		{
			*normal = -*normal;
		}
	}
}

impl BevyPhysicsHooks for OneWayHeightFieldFilter<'_, '_> {
	fn modify_solver_contacts(&self, ctx: ContactModificationContextView) {
		self.filter(
			ctx.collider1(),
			ctx.collider2(),
			&ctx.raw.manifold.local_n1,
			&ctx.raw.manifold.local_n2,
			// Normal in local space means we check against Rapier's default up vector
			&Vector::y(),
			&mut ctx.raw.normal,
		)
	}
}
