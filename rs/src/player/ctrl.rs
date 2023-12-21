use crate::{
	player::{
		input::PlayerAction,
		player_entity::{Controller, Root},
		BelongsToPlayer, G1, HOVER_HEIGHT, PLAYER_GRAVITY, SLIDE_ANGLE,
	},
	UP,
};
use bevy::prelude::*;
use bevy_rapier3d::{na::Vector3, prelude::*};
use enum_components::ERef;
use leafwing_abilities::prelude::*;

#[derive(Component, Default)]
pub struct CtrlState {
	pub grounded: bool,
}

#[derive(Component, Debug, Default, Copy, Clone, Reflect)]
pub struct CtrlVel {
	pub linvel: Vect,
	pub angvel: Vect,
}

pub fn repel_ground(
	mut ctx: ResMut<RapierContext>,
	mut q: Query<
		(
			&Parent,
			&GlobalTransform,
			&mut CtrlVel,
			&mut KinematicCharacterControllerOutput,
			&mut CtrlState,
		),
		ERef<Controller>,
	>,
	t: Res<Time>,
) {
	for (body_id, global, mut ctrl_vel, mut state, mut ctrl_state) in &mut q {
		let global = global.compute_transform();
		let col = Collider::ball(1.0);

		let max_toi = HOVER_HEIGHT;
		let result = ctx.cast_shape(
			global.translation + ((global.rotation * ctrl_vel.linvel) * t.delta_seconds()), // predict next frame
			global.rotation,
			-UP,
			&col,
			max_toi,
			true,
			QueryFilter::new()
				.exclude_sensors()
				.exclude_rigid_body(**body_id)
				.groups(CollisionGroups::new(G1, !G1)),
		);

		if let Some((
			id,
			Toi {
				toi,
				details: Some(details),
				..
			},
		)) = result
		{
			let angle = details.normal1.angle_between(UP);
			let x = max_toi - toi; // Spring compression distance
			let body = ctx.entity2body()[&id];
			ctx.bodies.get_mut(body).unwrap().apply_impulse_at_point(
				Vector3::from(global.rotation * details.normal2) * x * 100.0,
				details.witness1.into(),
				true,
			);
			if angle < SLIDE_ANGLE {
				ctrl_state.grounded = true;
				state.grounded = true;

				// Empirically decided
				let stiffness = 256.0;
				let damping = 0.98;

				// Think of the antigrav spring as radiating out in a spherical cone (sector),
				// so the spring coefficient for a constant-area contact patch follows the inverse square law,
				// not a constant like typical hooke's law springs.
				let repel_accel = x * x * stiffness;

				ctrl_vel.linvel.z += (PLAYER_GRAVITY * t.delta_seconds()) // Resist gravity for this frame
					+ repel_accel * f32::min(t.delta_seconds(), 0.125); // don't let low framerate eject player

				ctrl_vel.linvel.z *= damping;
			} else {
				ctrl_state.grounded = false;
				state.grounded = false;

				if ctrl_vel.linvel.z < 0.0 {
					let norm = global.rotation.inverse() * details.normal1; // To player's local space
					let v_dot_norm = ctrl_vel.linvel.dot(norm);
					if v_dot_norm < 0.0 {
						// Player is moving "into" the slope

						ctrl_vel.linvel -= v_dot_norm * norm; // Remove the penetrating part

						if ctrl_vel.linvel.z > 0.0 {
							// Remove any upwards sliding
							ctrl_vel.linvel.z = 0.0;
						}
					}
				} else {
					// Let player keep sliding up until gravity stops them
				}
			}
		} else {
			ctrl_state.grounded = false;
		}
	}
}

pub fn reset_jump_on_ground(mut q: Query<(AbilityState<PlayerAction>, &CtrlState)>) {
	for (mut state, ctrl_state) in &mut q {
		if ctrl_state.grounded
			&& state
				.cooldowns
				.get(PlayerAction::Jump)
				.as_ref()
				.unwrap()
				.ready()
				.is_ok()
		{
			let charges = state.charges.get_mut(PlayerAction::Jump).as_mut().unwrap();
			charges.set_charges(charges.max_charges());
		}
	}
}

pub fn gravity(mut q: Query<(&mut CtrlVel, &KinematicCharacterControllerOutput)>, t: Res<Time>) {
	for (mut ctrl_vel, out) in q.iter_mut() {
		if out.grounded {
			// Only when actual collider is grounded, not the antigrav pulses.
			// Let `repel_ground` handle counteracting gravity when hovering.
			ctrl_vel.linvel.z = f32::max(ctrl_vel.linvel.z, 0.0)
		}

		let decr = PLAYER_GRAVITY * t.delta_seconds();

		ctrl_vel.linvel.z -= decr;
	}
}

pub fn move_player(
	ctx: Res<RapierContext>,
	mut body_q: Query<(Entity, &mut Transform, &BelongsToPlayer), ERef<Root>>,
	mut ctrl_q: Query<
		(
			&CtrlVel,
			&mut KinematicCharacterController,
			&BelongsToPlayer,
		),
		ERef<Controller>,
	>,
	_sim_to_render: Res<SimulationToRenderTime>,
	t: Res<Time>,
) {
	let dt = t.delta_seconds();
	for (ctrl_vel, mut ctrl, ctrl_owner) in &mut ctrl_q {
		let (id, mut body_xform) = body_q
			.iter_mut()
			.find_map(|(id, xform, owner)|
				// Todo: cache owner->entity map
				(owner == ctrl_owner).then_some((id, xform)))
			.unwrap();

		let Some(&body) = ctx.entity2body().get(&id) else {
			continue;
		};
		let body = &ctx.bodies[body];
		let _body_rapier_pos = Vec3::from(*body.translation());

		// if let Some(end) = ctrl.translation {
		// 	vis_xform.translation = Vec3::ZERO.lerp(end, (DT + sim_to_render.diff) / DT);
		// 	// interp.end.map(|end| {
		// 	// 	let end = iso_to_transform(&end, 1.0);
		// 	// 	vis_xform.rotation = body_xform.rotation.slerp(end.rotation, (DT + sim_to_render.diff) / DT);
		// 	// 	vis_xform.translation = body_xform.translation.lerp(end.translation, (DT + sim_to_render.diff) / DT);
		// 	// 	// *vis_xform = iso_to_transform(&Isometry::default().lerp_slerp(&Isometry {
		// 	// 	// 	rotation: *Unit::from_ref_unchecked(&(*interp.end.unwrap().rotation * Quaternion::from(-body_xform.rotation))),
		// 	// 	// 	translation: (interp.end.unwrap().translation.vector - Translation::from(body_xform.translation).vector).into(),
		// 	// 	// }, sim_to_render.diff), 1.0);
		// 	// });
		// } else {
		// 	vis_xform.translation = Vec3::ZERO;
		// }

		// if let (Some(start), Some(end)) = (interp.start, interp.end) {
		// 	kin_interp.start = Some(start);
		// 	kin_interp.end = Some(end);
		// }
		// if let Some(interp) = kin_interp.lerp_slerp((DT + accum.diff) / DT) {
		// 	let new_xform = iso_to_transform(&interp, 1.0);
		// 	if *body_xform != new_xform {
		// 		*body_xform = new_xform;
		// 	};
		// }

		let rot = ctrl_vel.angvel * dt;
		let rot = Quat::from_euler(EulerRot::ZXY, rot.z, rot.x, rot.y);
		body_xform.rotate_local(rot);

		let slide = body_xform.rotation * ctrl_vel.linvel * dt;
		ctrl.translation = Some(slide);
	}
}

pub fn save_tmp_transform(
	mut cmds: Commands,
	mut q: Query<(Entity, &Transform, Option<&mut TmpPos>), ERef<Root>>,
) {
	for (id, xform, tmp) in &mut q {
		if let Some(mut tmp) = tmp {
			**tmp = xform.translation;
		} else {
			cmds.entity(id).insert(TmpPos(xform.translation));
		}
	}
}

pub fn load_tmp_transform(mut q: Query<(&mut Transform, &TmpPos), ERef<Root>>) {
	for (mut xform, tmp) in &mut q {
		xform.translation = **tmp;
	}
}

#[derive(Component, Debug, Default, Deref, DerefMut)]
pub struct TmpPos(Vec3);
