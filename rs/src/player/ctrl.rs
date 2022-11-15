use bevy::prelude::*;
use bevy_rapier3d::prelude::*;
use crate::player::{G1, HOVER_HEIGHT, SLIDE_ANGLE};
use crate::player::player_entity::{Controller, ReadPlayerEntity, Root};
use crate::UP;

#[derive(Component)]
pub struct CtrlState {
	pub grounded: bool,
}

pub fn repel_ground(
	ctx: Res<RapierContext>,
	mut root_q: Query<ReadPlayerEntity<Root>>,
	mut q: Query<(&Parent, &GlobalTransform, &Collider, &mut CtrlVel, &mut KinematicCharacterControllerOutput), ReadPlayerEntity<Controller>>,
	t: Res<Time>,
) {
	for (body_id, xform, col, mut ctrl_vel, mut state) in &mut q {
		let global = xform.compute_transform();
		let result = ctx.cast_shape(
			global.translation,
			global.rotation,
			-UP,
			col,
			HOVER_HEIGHT,
			QueryFilter::new()
				.groups(InteractionGroups::new(G1, !G1)),
		);
		if let Some((_, toi)) = result {
			let angle = toi.normal1.angle_between(UP);
			if angle <= SLIDE_ANGLE {
				state.grounded = true;
				ctrl_vel.linvel.z = f32::max((HOVER_HEIGHT - toi.toi) * 2.0, ctrl_vel.linvel.z)
			} else {
				let local_norm = -global.rotation * toi.normal1;
				let vert = local_norm * local_norm.dot(UP);
				let horiz = local_norm - vert;
				ctrl_vel.linvel += (HOVER_HEIGHT - toi.toi) * 2.0 * horiz;
			}
		}
	}
}

#[derive(Component, Debug, Default, Copy, Clone, Reflect)]
pub struct CtrlVel {
	pub linvel: Vect,
	pub angvel: Vect,
}
