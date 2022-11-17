use crate::player::player_entity::{Controller, ReadPlayerEntity};
use crate::player::{G1, HOVER_HEIGHT, SLIDE_ANGLE};
use crate::UP;
use bevy::prelude::*;
use bevy_rapier3d::prelude::*;
use crate::util::quantize;

#[derive(Component)]
pub struct CtrlState {
	pub grounded: bool,
}

pub fn repel_ground(
	ctx: Res<RapierContext>,
	mut q: Query<
		(
			&Parent,
			&GlobalTransform,
			&Collider,
			&mut CtrlVel,
			&mut KinematicCharacterControllerOutput,
		),
		ReadPlayerEntity<Controller>,
	>,
	t: Res<Time>,
) {
	for (body_id, global, col, mut ctrl_vel, mut state) in &mut q {
		let global = global.compute_transform();
		let result = ctx.cast_shape(
			global.translation,
			global.rotation,
			-UP,
			col,
			HOVER_HEIGHT,
			QueryFilter::new()
				.exclude_rigid_body(**body_id)
				.groups(InteractionGroups::new(G1, !G1)),
		);
		if let Some((_, toi)) = result {
			let angle = quantize::<16>(toi.normal1.angle_between(UP));
			if angle < SLIDE_ANGLE {
				state.grounded = true;
				let dist = HOVER_HEIGHT - toi.toi;
				let repel_accel = dist * dist * 64.0;
				let z = ctrl_vel.linvel.z;
				ctrl_vel.linvel.z = z + ((repel_accel - z) * t.delta_seconds() * 4.0)
			}
		}
	}
}

#[derive(Component, Debug, Default, Copy, Clone, Reflect)]
pub struct CtrlVel {
	pub linvel: Vect,
	pub angvel: Vect,
}
