use crate::{
	planet::{
		chunks::{ChunkIndex, TERRAIN_CELL_SIZE},
		PlanetVec2,
	},
	util::Prev,
};
use bevy::prelude::*;
use bevy_rapier3d::{
	dynamics::RapierRigidBodyHandle, na::Vector, plugin::RapierContext,
	prelude::TransformInterpolation,
};
use particles::{
	InitialGlobalTransform, InitialTransform, PreviousGlobalTransform, PreviousTransform,
};
use serde::{Deserialize, Serialize};

pub const DEFAULT_FRAME_RADIUS: f32 = 128.0 * TERRAIN_CELL_SIZE;

pub struct PlanetFramePlugin;

impl Plugin for PlanetFramePlugin {
	fn build(&self, app: &mut App) {
		app.add_systems(First, reframe_all_entities)
			.insert_resource(Frame::default())
			.insert_resource(Prev::<Frame>::default())
			.register_type::<Frame>();
	}
}

#[derive(Resource, Debug, Copy, Clone, PartialEq, Serialize, Deserialize, Reflect)]
#[reflect(Resource)]
pub struct Frame {
	pub center: PlanetVec2,
	pub trigger_bounds: f32,
	pub min_width: f32,
}

impl Frame {
	pub fn planet_coords_of(&self, point: Vec2) -> PlanetVec2 {
		self.center + point
	}
	pub fn closest_chunk(&self, point: Vec2) -> ChunkIndex {
		self.planet_coords_of(point).into()
	}
}

impl Default for Frame {
	fn default() -> Self {
		Self {
			center: default(),
			trigger_bounds: DEFAULT_FRAME_RADIUS,
			min_width: DEFAULT_FRAME_RADIUS,
		}
	}
}

pub fn reframe_all_entities(
	mut ctx: ResMut<RapierContext>,
	mut q: Query<(&mut Transform, &mut GlobalTransform, Has<Parent>)>,
	bodies: Query<&RapierRigidBodyHandle>,
	mut interpolations: Query<&mut TransformInterpolation, Without<Parent>>,
	mut prev_xforms: Query<&mut Prev<Transform>, Without<Parent>>,
	mut prev_globals: Query<&mut Prev<GlobalTransform>>,
	mut prev_particles: Query<(
		&mut PreviousTransform,
		&mut PreviousGlobalTransform,
		Has<Parent>,
	)>,
	mut init_particles: Query<(
		&mut InitialTransform,
		&mut InitialGlobalTransform,
		Has<Parent>,
	)>,
	frame: Res<Frame>,
	prev_frame: Res<Prev<Frame>>,
) {
	if !frame.is_changed() || frame.is_added() || frame.center == prev_frame.center {
		return;
	}
	info!("{frame:?}");
	let diff = Vec2::from(frame.center - prev_frame.center);
	let offset = Vec3::new(-diff.x, -diff.y, 0.0);
	q.par_iter_mut()
		.for_each(|(mut xform, mut global, has_parent)| {
			if !has_parent {
				xform.translation += offset;
			}
			let mut global_xform = global.compute_transform();
			global_xform.translation += offset;
			*global = GlobalTransform::from(global_xform);
		});
	prev_xforms.par_iter_mut().for_each(|mut prev_xform| {
		prev_xform.translation += offset;
	});
	prev_globals.par_iter_mut().for_each(|mut prev_global| {
		let mut computed = prev_global.compute_transform();
		computed.translation += offset;
		**prev_global = GlobalTransform::from(computed);
	});
	prev_particles
		.par_iter_mut()
		.for_each(|(mut prev_xform, mut prev_global, has_parent)| {
			if !has_parent {
				prev_xform.translation += offset;
			}
			let mut computed = prev_global.compute_transform();
			computed.translation += offset;
			**prev_global = GlobalTransform::from(computed);
		});
	init_particles
		.par_iter_mut()
		.for_each(|(mut init_xform, mut init_global, has_parent)| {
			if !has_parent {
				init_xform.translation += offset;
			}
			let mut computed = init_global.compute_transform();
			computed.translation += offset;
			**init_global = GlobalTransform::from(computed);
		});

	// Rapier transforms
	let offset = Vector::from(offset);
	for body in &bodies {
		// TODO: Parallelize this -- RigidBodySet does not have par_iter
		if let Some(body) = ctx.bodies.get_mut(body.0) {
			let pos = body.translation();
			body.set_translation(*pos + offset, false);
		}
	}
	interpolations.par_iter_mut().for_each(|mut interp| {
		if let Some(start) = interp.start.as_mut() {
			start.translation.vector += offset;
		}
		if let Some(end) = interp.end.as_mut() {
			end.translation.vector += offset;
		}
	});
}
