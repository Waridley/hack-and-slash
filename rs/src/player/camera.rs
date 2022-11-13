use super::player_entity::{Cam, CamPivot, ReadPlayerEntity};
use super::{BelongsToPlayer, G1};
use bevy::{
	ecs::entity::Entity,
	ecs::system::Res,
	prelude::*,
	transform::components::{GlobalTransform, Transform},
};
use bevy_rapier3d::{geometry::Collider, math::Vect, pipeline::QueryFilter, plugin::RapierContext};
use rapier3d::geometry::InteractionGroups;

pub const CAM_ACCEL: f32 = 12.0;
const MAX_CAM_DIST: f32 = 24.0;
const MIN_CAM_DIST: f32 = 3.2;

#[derive(Component, Debug, Copy, Clone, Deref, DerefMut, Reflect)]
pub struct Pivot(pub Entity);

#[derive(Component, Debug, Default, Copy, Clone, Reflect)]
pub struct CameraVertSlider(pub f32);

#[derive(Debug, Default, Component, Deref, DerefMut)]
pub struct CamTarget(Transform);

pub fn position_camera_target(
	ctx: Res<RapierContext>,
	cam_pivot_q: Query<(&GlobalTransform, &BelongsToPlayer), ReadPlayerEntity<CamPivot>>,
	mut cam_q: Query<(&mut CamTarget, &Collider, &BelongsToPlayer), ReadPlayerEntity<Cam>>,
) {
	for (mut target, col, cam_owner) in &mut cam_q {
		let pivot_xform = cam_pivot_q
			.iter()
			.find_map(|(xform, owner)| (owner == cam_owner).then_some(xform))
			.unwrap();

		let filter = QueryFilter::from(InteractionGroups::new(G1, !G1));
		let (_, rot, tr) = pivot_xform.to_scale_rotation_translation();
		let dir = rot * Vect::NEG_Y;
		let pos = tr + (dir * MIN_CAM_DIST); // start at minimum distance, not player origin
		let result = ctx.cast_shape(pos, -rot, dir, col, MAX_CAM_DIST - MIN_CAM_DIST, filter);
		let toi = if let Some((_, toi)) = result {
			let toi = toi.toi;
			if toi == 0.0 {
				if ctx
					.cast_shape(
						tr + dir * MAX_CAM_DIST,
						-rot,
						-dir,
						col,
						(MAX_CAM_DIST - MIN_CAM_DIST) * 0.3, // Don't want enormous object right in front of camera if possible
						filter,
					)
					.is_some()
				{
					// Max distance is still inside something, just let it be close
					toi
				} else {
					// Something is in-between character and camera, but better than camera being inside it
					MAX_CAM_DIST
				}
			} else {
				toi
			}
		} else {
			MAX_CAM_DIST
		};
		target.translation = tr + dir * (toi + MIN_CAM_DIST);
		target.rotation = -rot;
	}
}

pub fn follow_camera_target(
	mut cam_q: Query<(&mut Transform, &CamTarget), ReadPlayerEntity<Cam>>,
	t: Res<Time>,
) {
	let dt = t.delta_seconds();
	for (mut cam_xform, target_xform) in &mut cam_q {
		// TODO: Maybe always aim towards pivot, rather than immediately assuming final rotation
		cam_xform.translation = cam_xform
			.translation
			.lerp(target_xform.translation, CAM_ACCEL * dt);
		cam_xform.rotation = cam_xform
			.rotation
			.slerp(target_xform.rotation, CAM_ACCEL * dt);
	}
}
