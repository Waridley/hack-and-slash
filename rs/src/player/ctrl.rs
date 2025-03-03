use bevy::{math::bounding::BoundingSphere, prelude::*};
use bevy_rapier3d::{
	na::Vector3,
	parry::{
		query::{DefaultQueryDispatcher, PersistentQueryDispatcher},
		shape::Capsule,
	},
	prelude::*,
};
use enum_components::WithVariant;
use rapier3d::{
	math::Isometry,
	parry::query::ContactManifold,
	prelude::{ContactData, ContactManifoldData},
};

use crate::{
	player::{
		abilities::BoosterCharge,
		player_entity::{Controller, Root, ShipCenter},
		tune::PlayerParams,
		BelongsToPlayer, G1,
	},
	UP,
};

#[derive(Component, Default, Clone, Debug, Reflect)]
pub struct CtrlState {
	pub touching_ground: bool,
	pub bottomed_out: bool,
}

impl CtrlState {
	pub fn bottom_out(&mut self) {
		self.touching_ground = true;
		self.bottomed_out = true;
	}
}

#[derive(Component, Debug, Default, Copy, Clone, Reflect)]
pub struct CtrlVel {
	pub linvel: Vect,
	pub angvel: Vect,
}

// pub fn repel_ground(
// 	mut ctx: ResMut<RapierContext>,
// 	mut q: Query<
// 		(
// 			&Parent,
// 			&GlobalTransform,
// 			&Collider,
// 			&mut CtrlVel,
// 			&mut CtrlState,
// 		),
// 		ERef<Controller>,
// 	>,
// 	params: Res<PlayerParams>,
// ) {
// 	for (body_id, global, col, ctrl_vel, ctrl_state) in &mut q {
// 		player_repel_ground(
// 			ctx.reborrow(),
// 			body_id,
// 			global,
// 			col,
// 			ctrl_vel,
// 			ctrl_state,
// 			&*params,
// 			crate::DT,
// 		)
// 	}
// }

pub fn player_repel_ground(
	mut ctx: Mut<RapierContext>,
	body_id: &Parent,
	global: &GlobalTransform,
	col: &Collider,
	mut ctrl_vel: Mut<CtrlVel>,
	mut ctrl_state: Mut<CtrlState>,
	params: &PlayerParams,
	dt: f32,
) {
	let global = global.compute_transform();

	let max_toi = params.phys.hover_height;
	let result = ctx.cast_shape(
		global.translation,
		global.rotation,
		-UP,
		col,
		ShapeCastOptions {
			max_time_of_impact: max_toi,
			..default()
		},
		QueryFilter::new()
			.exclude_sensors()
			.exclude_rigid_body(**body_id)
			.groups(CollisionGroups::new(G1, !G1)),
	);

	if let Some((
		id,
		ShapeCastHit {
			time_of_impact: toi,
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
		if angle < params.phys.slide_angle.rad() {
			ctrl_state.touching_ground = true;

			// Empirically decided
			let stiffness = 256.0;
			let damping = 4.0;

			// Think of the antigrav spring as radiating out in a spherical cone (sector),
			// so the spring coefficient for a constant-area contact patch follows the inverse square law,
			// not a constant like typical hooke's law springs.
			let repel_accel = x * x * stiffness;

			ctrl_vel.linvel.z += (params.phys.gravity * dt) // Resist gravity for this frame
				+ repel_accel * f32::min(dt, 0.125); // don't let low framerate eject player

			ctrl_vel.linvel.z *= 1.0 - (damping * dt);
		} else {
			ctrl_state.touching_ground = false;

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
		ctrl_state.touching_ground = false;
	}
}

pub fn reset_jump_on_ground(
	mut q: Query<(&CtrlState, &mut BoosterCharge), Changed<CtrlState>>,
	params: Res<PlayerParams>,
) {
	let grounded_charge = params.abil.grounded_booster_charge;
	for (ctrl_state, mut charge) in &mut q {
		if ctrl_state.touching_ground && *charge < grounded_charge {
			*charge = grounded_charge
		}
	}
}

pub fn gravity(mut q: Query<(&mut CtrlVel, &CtrlState)>, params: Res<PlayerParams>, t: Res<Time>) {
	for (mut ctrl_vel, state) in q.iter_mut() {
		if state.bottomed_out {
			// Only when actual collider is grounded, not the antigrav pulses.
			// Let `repel_ground` handle counteracting gravity when hovering.
			ctrl_vel.linvel.z = f32::max(ctrl_vel.linvel.z, 0.0)
		}

		let decr = params.phys.gravity * t.delta_secs();

		ctrl_vel.linvel.z -= decr;
	}
}

pub fn move_player(
	mut ctx: Single<&mut RapierContext>,
	mut body_q: Query<
		(Entity, &mut Transform, &GlobalTransform, &BelongsToPlayer),
		WithVariant<Root>,
	>,
	mut ctrl_q: Query<
		(
			Entity,
			&Parent,
			&GlobalTransform,
			&mut CtrlVel,
			&Collider,
			&BelongsToPlayer,
			&mut CtrlState,
		),
		WithVariant<Controller>,
	>,
	mut vis_q: Query<
		(
			&mut Transform,
			&mut TransformInterpolation,
			&BelongsToPlayer,
		),
		WithVariant<ShipCenter>,
	>,
	params: Res<PlayerParams>,
	sim_to_render: Single<&SimulationToRenderTime>,
	t: Res<Time>,
) {
	let dt = crate::DT;
	let mut diff = sim_to_render.diff + t.delta_secs();
	let mut iters = 4;
	while diff > 0.0 && iters > 0 {
		diff -= dt;
		iters -= 1;

		for (col_id, body_id, global, mut ctrl_vel, col, ctrl_owner, mut ctrl_state) in &mut ctrl_q
		{
			let Some(&body_handle) = ctx.entity2body().get(body_id) else {
				continue;
			};
			player_repel_ground(
				ctx.reborrow(),
				body_id,
				global,
				col,
				ctrl_vel.reborrow(),
				ctrl_state.reborrow(),
				&params,
				dt,
			);

			let Some((body_id, mut body_xform, body_global)) =
				body_q.iter_mut().find_map(|(id, xform, global, owner)|
					// Todo: cache owner->entity map
					(owner == ctrl_owner).then_some((id, xform, global.compute_transform())))
			else {
				return;
			};
			let (mut vis_xform, mut vis_interp) = vis_q
				.iter_mut()
				.find_map(|(xform, interp, owner)| (owner == ctrl_owner).then_some((xform, interp)))
				.unwrap();

			let rot = ctrl_vel.angvel * dt;
			let rot = Quat::from_euler(EulerRot::ZXY, rot.z, rot.x, rot.y);
			body_xform.rotate_local(rot);

			let vel = body_xform.rotation * ctrl_vel.linvel;

			let filter = QueryFilter {
				flags: QueryFilterFlags::EXCLUDE_SENSORS,
				groups: Some(CollisionGroups::new(G1, !G1)),
				exclude_collider: None,
				exclude_rigid_body: Some(body_id),
				predicate: None,
			};

			const BUFFER: f32 = 0.1;

			let mut rem = dt;
			let mut result = Vec3::ZERO;
			let mut dir = vel;
			let mut manifolds: Vec<ContactManifold<ContactManifoldData, ContactData>> = vec![];
			let mut contacts = vec![];
			let mut pen_fix_iters = 20;
			while rem > dt * BUFFER {
				// Using custom shape casting because of too many bugs/design choices I don't want with rapier's character controller.
				match ctx.cast_shape(
					body_global.translation + result,
					body_global.rotation,
					dir,
					col,
					ShapeCastOptions {
						max_time_of_impact: rem * (1.0 + BUFFER),
						..default()
					},
					filter,
				) {
					Some((
						_,
						ShapeCastHit {
							time_of_impact: toi,
							details: Some(hit),
							status: _status,
						},
					)) => {
						let safe_toi = (toi - (dt * BUFFER)).clamp(0.0, rem);
						let penetrating_part = dir * ((rem - toi) / rem);
						let reaction = -penetrating_part.dot(hit.normal1) * (hit.normal1 * 1.01);
						let slide_dir = penetrating_part + reaction;

						let angle = hit.normal1.angle_between(UP);
						if angle < params.phys.slide_angle.rad() {
							ctrl_state.bottom_out();
							// Let player slide like normal
							result += dir * safe_toi;
							dir = slide_dir;
							rem -= toi;
						} else {
							ctrl_state.bottomed_out = false;

							let already_moving_up = vel.z > BUFFER;

							let v_dot_norm = dir.dot(hit.normal1);
							if v_dot_norm <= 0.0 {
								// Player is moving "into" the slope

								let slide_dir = if !already_moving_up && hit.normal1.z > -BUFFER {
									// Don't slide up a steep slope.
									Vec3::new(slide_dir.x, slide_dir.y, f32::min(0.0, slide_dir.z))
								} else {
									// Either let player keep moving up due to their existing velocity,
									// or let player slide down the slope.
									slide_dir
								};
								result += dir * safe_toi;
								dir = slide_dir;
								rem -= toi;
							} else {
								// I don't think this can happen if `details` exists, but if it does, it means the Player is moving
								// away from the slope, probably penetrating, and so we want to get out of that state.
								#[cfg(debug_assertions)]
								warn!("I thought this was unreachable. Status: {_status:?}");
								result += dir * rem;
								rem = 0.0;
								dir = Vec3::ZERO;
							}
						}
					}
					Some((
						_,
						ShapeCastHit {
							status: ShapeCastStatus::PenetratingOrWithinTargetDist,
							..
						},
					)) => {
						let q = DefaultQueryDispatcher;
						contacts.clear();
						let pos = body_global.translation + result;
						let iso = Isometry::new(pos.into(), UP.into());
						let ball = col.as_ball().unwrap();
						let aabb = BoundingSphere::new(pos, ball.raw.radius + (vel * dt).length())
							.aabb_3d();

						// Attempt to prevent tunnelling by using an extruded sphere (capsule) instead
						let capsule =
							Capsule::new((-vel * dt).into(), Vec3::ZERO.into(), ball.raw.radius);

						ctx.colliders_with_aabb_intersecting_aabb(aabb, |handle| {
							if handle == col_id {
								return true;
							}
							let Some(handle) = ctx.entity2collider().get(&handle) else {
								return true;
							};
							if let Some(other_col) = ctx.colliders.get(*handle) {
								manifolds.clear();
								let rel = iso.inv_mul(other_col.position());
								if q.contact_manifolds(
									&rel,
									&capsule,
									other_col.shape(),
									BUFFER,
									&mut manifolds,
									&mut None,
								)
								.is_err()
								{
									warn!(
										"Cannot check if player is penetrating {:?}",
										other_col.shape().shape_type()
									)
								}
								for manifold in &manifolds {
									let Some(contact) = manifold.find_deepest_contact() else {
										continue;
									};
									let dist = contact.dist;
									if dist >= 0.0 {
										continue;
									}
									let norm = Vec3::from(manifold.local_n1);
									if norm.angle_between(UP) < params.phys.slide_angle.rad() {
										// Any shallow-enough slope bottoms us out
										ctrl_state.bottom_out()
									} else {
										// // Don't push player up steep slope
										// norm.z = f32::min(0.0, norm.z);
										// let len = norm.length();
										// dist /= len;
										// norm /= len;
									}
									contacts.push((norm, dist));
								}
							}
							true
						});
						let mut fix = Vec3::ZERO;
						for (dir, dist) in contacts.drain(..) {
							fix += dir * f32::min(dt, dist * (1.0 + BUFFER));
						}
						if fix.angle_between(UP) < params.phys.slide_angle.rad() {
							// Even if individual surfaces were all too steep, getting stuck in a V should still bottom us out.
							ctrl_state.bottom_out()
						}
						result += dbg!(fix);
						pen_fix_iters -= 1;
						if pen_fix_iters == 0 {
							rem = 0.0;
						}
					}
					toi => {
						if let Some(toi) = toi {
							dbg!(toi);
						}
						ctrl_state.bottomed_out = false;
						result += dir * rem;
						rem = 0.0;
						dir = Vec3::ZERO;
					}
				}
			}
			let body = ctx.bodies.get_mut(body_handle).unwrap();
			body.set_rotation(body_xform.rotation.into(), true);
			if result.length() > 0.0 {
				vis_xform.translation -= result;
				let end = vis_interp.end.unwrap();
				vis_interp.start = Some(Isometry::new(
					end.translation.vector - Vector3::from(body_xform.rotation.inverse() * result),
					(Quat::from(end.rotation).inverse() * rot.inverse())
						.to_scaled_axis()
						.into(),
				));
				body_xform.translation += result;
				body.set_translation(body_xform.translation.into(), true);
			}
		}
	}
	for (mut xform, interp, _) in &mut vis_q {
		let new = interp
			.lerp_slerp(1.0 + diff / dt)
			.unwrap_or_else(|| interp.end.unwrap());
		xform.translation = new.translation.into();
		xform.rotation = new.rotation.into();
	}
}
