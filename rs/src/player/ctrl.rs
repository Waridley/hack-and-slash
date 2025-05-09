use crate::{
	player::{
		abilities::BoosterCharge,
		player_entity::{CamPivot, Controller, Root, ShipCenter},
		tune::PlayerParams,
		BelongsToPlayer, G1,
	},
	UP,
};
use bevy::{math::bounding::BoundingSphere, prelude::*};
use bevy_rapier3d::{
	na::Vector3,
	parry::{
		query::{DefaultQueryDispatcher, PersistentQueryDispatcher},
		shape::Capsule,
	},
	prelude::*,
};
use engine::{
	planet::terrain::physics::OneWayHeightFieldFilter,
	util::{LerpSmoothing, SlerpSmoothing},
};
use enum_components::WithVariant;
use rapier3d::{
	math::Isometry,
	na::UnitQuaternion,
	parry::query::ContactManifold,
	prelude::{ContactData, ContactManifoldData},
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

pub fn antigrav(
	mut ctx: Mut<RapierContext>,
	body_id: Entity,
	global: &Transform,
	col: &Collider,
	mut ctrl_vel: Mut<CtrlVel>,
	mut ctrl_state: Mut<CtrlState>,
	params: &PlayerParams,
) {
	use engine::DT;

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
			.exclude_rigid_body(body_id)
			.groups(CollisionGroups::new(G1, !G1)),
	);

	// Think of the antigrav spring as radiating out in a spherical cone (sector),
	// so the spring coefficient for a constant-area contact patch follows the inverse square law,
	// not a constant like typical hooke's law springs.
	// This function returns the acceleration at compression distance `x` without incorporating mass.
	fn f(x: f32) -> f32 {
		x * x
	}

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
		let x0 = (max_toi - toi).clamp(0.0, max_toi); // Initial spring compression distance
		let Some(&body) = ctx.entity2body().get(&id) else {
			error!(?id, "body hit by antigrav missing from ctx.entity2body");
			return;
		};
		ctx.bodies.get_mut(body).unwrap().apply_impulse_at_point(
			Vector3::from(global.rotation * details.normal2) * x0 * 100.0,
			details.witness1.into(),
			true,
		);
		if angle < params.phys.slide_angle.rad() {
			ctrl_state.touching_ground = true;

			let stiffness = params.phys.antigrav_stiffness;
			let damping = params.phys.antigrav_spring_damping;

			// Midpoint approximation of integral of acceleration over the predicted distance.
			let dx_dt = -ctrl_vel.linvel.z; // moving downwards increases compression of spring
			let x1 = x0 + (dx_dt * DT);
			let (x1, t_used) = if dx_dt.abs() <= crate::EPS {
				(x0, DT)
			} else if x1 < crate::EPS {
				(0.0, (x0 / -dx_dt) * DT)
			} else if x1 > max_toi {
				(max_toi, ((max_toi - x0) / (x1 - x0)) * DT)
			} else {
				(x1, DT)
			};
			let integral = (f(x0) + f(x1)) * 0.5;
			let repel_accel = integral * stiffness;
			debug_assert!(t_used >= 0.0);

			// ctrl_vel.linvel.z += (params.phys.gravity * DT); // Resist gravity for this frame
			ctrl_vel.linvel.z += repel_accel * t_used;

			ctrl_vel.linvel.z = ctrl_vel.linvel.z.exp_decay(0.0, damping, DT);
		} else if angle > std::f32::consts::FRAC_PI_2 + crate::EPS {
			error!(
				?details,
				"Downward shape cast shouldn't hit downward-facing surfaces"
			);
			return;
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
			trace!("Bottomed out, resetting gravity");
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
	heightfield_filter: OneWayHeightFieldFilter,
	mut body_q: Query<(Entity, &mut Transform, &BelongsToPlayer), WithVariant<Root>>,
	mut ctrl_q: Query<
		(
			Entity,
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
	mut pivot_q: Query<(Ref<Transform>, &BelongsToPlayer), WithVariant<CamPivot>>,
	params: Res<PlayerParams>,
	sim_to_render: Single<&SimulationToRenderTime>,
	t: Res<Time>,
	mut manifolds: Local<Vec<ContactManifold<ContactManifoldData, ContactData>>>,
	mut contacts: Local<Vec<(Vec3, f32)>>,
) {
	use engine::DT;
	// Rapier adds t.delta_secs() at the beginning of the simulation step, but we need to know what
	// it is before that happens, while still matching the behavior of other interpolated bodies.
	let mut diff = sim_to_render.diff + t.delta_secs();
	let mut iters = 4;
	while diff > 0.0 && iters > 0 {
		diff -= DT;
		iters -= 1;

		for (col_id, mut ctrl_vel, col, ctrl_owner, mut ctrl_state) in &mut ctrl_q {
			let Some((body_id, mut body_xform)) =
				body_q.iter_mut().find_map(|(id, xform, owner)|
					// Todo: cache owner->entity map
					(owner == ctrl_owner).then_some((id, xform)))
			else {
				return;
			};
			let Some(pivot_xform) = pivot_q
				.iter_mut()
				.find_map(|(xform, owner)| (owner == ctrl_owner).then_some(xform))
			else {
				return;
			};
			let (pivot_yaw, _, _) = pivot_xform.rotation.to_euler(EulerRot::ZXY);
			let prev_rot = body_xform.rotation;

			let vel = body_xform.rotation * ctrl_vel.linvel;

			let filter = QueryFilter {
				flags: QueryFilterFlags::EXCLUDE_SENSORS,
				groups: Some(CollisionGroups::new(G1, !G1)),
				exclude_collider: None,
				exclude_rigid_body: Some(body_id),
				predicate: None,
			};

			const BUFFER: f32 = 0.1;

			let mut rem = DT;
			let mut result = Vec3::ZERO;
			let mut dir = vel;
			let mut attempts = 8;
			while rem > DT * BUFFER {
				if attempts <= 0 {
					break;
				}
				attempts -= 1;

				let (_yaw, pitch, roll) = body_xform.rotation.to_euler(EulerRot::ZXY);
				let target = Quat::from_euler(EulerRot::ZXY, pivot_yaw, pitch, roll);
				let new = body_xform
					.rotation
					.sph_exp_decay(&target, params.anim.turn_lambda, DT);
				if body_xform.rotation != new {
					body_xform.rotation = new;
				}

				antigrav(
					ctx.reborrow(),
					body_id,
					&body_xform,
					col,
					ctrl_vel.reborrow(),
					ctrl_state.reborrow(),
					&params,
				);

				// Using custom shape casting because of too many bugs/design choices I dislike in rapier's character controller.
				match ctx.cast_shape(
					body_xform.translation + result,
					body_xform.rotation,
					dir,
					col,
					ShapeCastOptions {
						max_time_of_impact: rem,
						..default()
					},
					filter,
				) {
					Some((
						hit_collider,
						ShapeCastHit {
							time_of_impact: toi,
							details: Some(mut hit),
							status: _status,
						},
					)) => {
						if heightfield_filter.should_flip_normal(
							hit_collider,
							&hit.normal1.into(),
							&UP.into(),
						) {
							hit.normal1 = -hit.normal1;
							hit.normal2 = -hit.normal2;
						}

						let safe_toi = (toi - (DT * BUFFER)).clamp(-BUFFER, rem);
						let penetrating_part = dir * ((rem - toi) / rem);
						let reaction =
							-penetrating_part.dot(hit.normal1) * (hit.normal1 * (1.0 + BUFFER));
						let slide_dir = penetrating_part + reaction;

						let angle = hit.normal1.angle_between(UP);
						if angle < params.phys.slide_angle.rad() {
							// Can slide up slope
							trace!(attempt=?(8 - attempts), ?dir, ?slide_dir, ?hit.normal1, rem, toi);
							ctrl_state.bottom_out();
							// Let player slide like normal
							result += dir * safe_toi;
							dir = slide_dir;
							rem -= toi;
							// Gravity system isn't always seeing `bottomed_out`, probably due to fixed timestep.
							// Maybe it should be an event instead?
							// TODO: IF I ever want bouncy objects, this might need to take restitution into account.
							ctrl_vel.linvel.z = f32::max(ctrl_vel.linvel.z, BUFFER);
						} else {
							// Can't slide up slope unless we already have positive vertical velocity
							ctrl_state.bottomed_out = false;

							let already_moving_up = vel.z > crate::EPS;

							let v_dot_norm = dir.dot(hit.normal1);
							trace!(attempt=?(8 - attempts), ?already_moving_up, ?v_dot_norm);
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
								// Player is moving away from the slope, probably penetrating, so we want to get out
								// of that state.
								result += dir * rem;
								rem = 0.0;
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
						let pos = body_xform.translation + result;
						let iso = Isometry::new(pos.into(), UP.into());
						let ball = col.as_ball().unwrap();
						let aabb = BoundingSphere::new(pos, ball.raw.radius + (vel * DT).length())
							.aabb_3d();

						// Attempt to prevent tunnelling by using an extruded sphere (capsule) instead
						let capsule =
							Capsule::new((-vel * DT).into(), Vec3::ZERO.into(), ball.raw.radius);

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
							fix += dir * f32::min(DT, dist * (1.0 + BUFFER));
						}
						if fix.angle_between(UP) < params.phys.slide_angle.rad() {
							// Even if individual surfaces were all too steep, getting stuck in a V should still bottom us out.
							ctrl_state.bottom_out()
						}
						trace!(?fix);
						result += fix;
					}
					toi => {
						trace!(attempt=?(8 - attempts), ?dir, rem, ?toi);
						ctrl_state.bottomed_out = false;
						result += dir * rem;
						rem = 0.0;
						dir = Vec3::ZERO;
					}
				}
			}
			if result.length() > 0.0 {
				let mut vis_interp = vis_q
					.iter_mut()
					.find_map(|(_, interp, owner)| (owner == ctrl_owner).then_some(interp))
					.unwrap();
				let inv_rot = body_xform.rotation.inverse();
				let end = vis_interp.end.unwrap();
				// vis_interp.end is always the same, resting transform.
				vis_interp.start = Some(Isometry {
					translation: (end.translation.vector - Vector3::from(inv_rot * result)).into(),
					rotation: end.rotation * UnitQuaternion::from(inv_rot * prev_rot),
				});
				body_xform.translation += result;
			}
		}
	}
	for (mut xform, interp, _) in &mut vis_q {
		let new = interp
			.lerp_slerp((DT + diff) / DT)
			.unwrap_or_else(|| interp.end.unwrap());
		xform.translation = new.translation.into();
		xform.rotation = new.rotation.into();
	}
}
