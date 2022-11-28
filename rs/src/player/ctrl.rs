use crate::{
	player::{
		input::PlayerAction,
		player_entity::{Controller, ReadPlayerEntity, Root},
		BelongsToPlayer, G1, HOVER_HEIGHT, PLAYER_GRAVITY, SLIDE_ANGLE,
	},
	util::truncate_mantissa,
	UP,
};
use bevy::prelude::*;
use bevy_rapier3d::prelude::*;
use leafwing_abilities::AbilityState;

#[derive(Component)]
pub struct CtrlState {
	pub grounded: bool,
}

#[derive(Component, Debug, Default, Copy, Clone, Reflect)]
pub struct CtrlVel {
	pub linvel: Vect,
	pub angvel: Vect,
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
				.exclude_sensors()
				.exclude_rigid_body(**body_id)
				.groups(InteractionGroups::new(G1, !G1)),
		);
		if let Some((_, toi)) = result {
			let angle = truncate_mantissa::<10>(toi.normal1.angle_between(UP));
			if angle < SLIDE_ANGLE {
				state.grounded = true;
				let dist = HOVER_HEIGHT - toi.toi;
				let repel_accel = dist * dist * 64.0 - ctrl_vel.linvel.z;
				ctrl_vel.linvel.z += repel_accel * f32::min(t.delta_seconds() * 2.0, 0.256);
			}
		}
	}
}

pub fn reset_jump_on_ground(
	mut q: Query<(
		AbilityState<PlayerAction>,
		&KinematicCharacterControllerOutput,
	)>,
) {
	for (mut state, out) in &mut q {
		if out.grounded {
			let charges = state.charges.get_mut(PlayerAction::Jump).as_mut().unwrap();
			charges.set_charges(charges.max_charges());
		}
	}
}

pub fn gravity(mut q: Query<(&mut CtrlVel, &KinematicCharacterControllerOutput)>, t: Res<Time>) {
	for (mut ctrl_vel, out) in q.iter_mut() {
		if out.grounded {
			ctrl_vel.linvel.z = 0.0
		}

		let mut info = vec![(TOIStatus::Converged, Vect::NAN, Vect::NAN); 4];
		for (i, col) in out.collisions.iter().enumerate() {
			if let Some(slot) = info.get_mut(i) {
				*slot = (col.toi.status, col.translation_remaining, col.toi.normal1)
			}
		}

		let decr = PLAYER_GRAVITY * t.delta_seconds();

		ctrl_vel.linvel.z -= decr;
	}
}

pub fn move_player(
	mut body_q: Query<(&mut Transform, &BelongsToPlayer), ReadPlayerEntity<Root>>,
	mut ctrl_q: Query<
		(
			&CtrlVel,
			&mut KinematicCharacterController,
			&BelongsToPlayer,
		),
		ReadPlayerEntity<Controller>,
	>,
	t: Res<Time>,
) {
	for (ctrl_vel, mut ctrl, ctrl_owner) in &mut ctrl_q {
		let mut body_xform = body_q
			.iter_mut()
			.find_map(|(xform, owner)| (owner == ctrl_owner).then_some(xform))
			.unwrap();

		let dt = t.delta_seconds();

		let Vec3 { x, y, z } = ctrl_vel.angvel * dt;
		let rot = Quat::from_euler(EulerRot::ZXY, z, x, y);
		body_xform.rotate_local(rot);

		let slide = body_xform.rotation * (ctrl_vel.linvel * dt);
		ctrl.translation = Some(slide);
	}
}
