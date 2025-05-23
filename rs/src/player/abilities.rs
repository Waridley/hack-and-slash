use web_time::Duration;

use bevy::{prelude::*, transform::TransformSystem::TransformPropagate};
#[cfg(feature = "bevy_kira_audio")]
use bevy_kira_audio::{Audio, AudioControl, AudioSource};
use bevy_rapier3d::{
	geometry::CollisionGroups,
	pipeline::QueryFilter,
	plugin::RapierContext,
	prelude::{Collider, ShapeCastHit, ShapeCastOptions},
};
use enum_components::{ERef, WithVariant};
use leafwing_input_manager::{action_state::ActionState, systems::update_action_state};
use particles::{PreviousGlobalTransform, Spewer};
use rapier3d::pipeline::QueryFilterFlags;
use serde::{Deserialize, Serialize};

use crate::{
	anim::{AnimationSet, BlendTargets, ComponentDelta, StartAnimation},
	planet::{
		chunks::{ChunkIndex, LoadedChunks},
		frame::Frame,
	},
	player::{
		ctrl,
		ctrl::CtrlVel,
		idle, input,
		input::PlayerAction,
		player_entity::{AntigravParticles, Arm, Arms, CamPivot, Orb},
		tune::{AbilityParams, PlayerParams},
		BelongsToPlayer, PlayerArm, PlayerId, RotVel,
	},
	terminal_velocity,
	util::{Diff, DurationDelta, Easings, LerpSlerp, Target},
};

pub struct AbilitiesPlugin;

impl Plugin for AbilitiesPlugin {
	fn build(&self, app: &mut App) {
		app.add_systems(
			Update,
			((trigger_player_abilities.after(idle), fill_weapons)
				.chain()
				.after(input::InputSystems)
				.after(ctrl::gravity)
				.after(update_action_state::<PlayerAction>)
				.before(terminal_velocity)
				.run_if(resource_exists::<PlayerParams>),),
		)
		.add_systems(
			PostUpdate,
			(hit_stuff
				.after(AnimationSet::<Transform>::SET)
				.after(TransformPropagate),),
		)
		.add_event::<Hurt>();
	}
}

pub fn trigger_player_abilities(
	mut cmds: Commands,
	mut q: Query<(
		&ActionState<PlayerAction>,
		&mut BoosterCharge,
		&mut WeaponCharge,
		&mut CtrlVel,
		&BelongsToPlayer,
	)>,
	arms_q: Query<(Entity, &RotVel, &BelongsToPlayer), WithVariant<Arms>>,
	arm_q: Query<(
		Entity,
		&Transform,
		&GlobalTransform,
		&BelongsToPlayer,
		ERef<Arm>,
	)>,
	orb_q: Query<(
		Entity,
		&Transform,
		&GlobalTransform,
		&Spewer,
		&BelongsToPlayer,
		ERef<Orb>,
	)>,
	chunk_q: Query<&GlobalTransform, With<ChunkIndex>>,
	cam_pivot_q: Query<(&GlobalTransform, &BelongsToPlayer), WithVariant<CamPivot>>,
	antigrav_q: Query<(Entity, &BelongsToPlayer), WithVariant<AntigravParticles>>,
	#[cfg(feature = "bevy_kira_audio")] sfx: Res<Sfx>,
	#[cfg(feature = "bevy_kira_audio")] audio: Res<Audio>,
	params: Res<PlayerParams>,
	frame: Res<Frame>,
	loaded_chunks: Res<LoadedChunks>,
) {
	// TODO: Cooldowns
	use PlayerAction::*;
	let AbilityParams {
		jump_cost,
		jump_vel,
		dash_cost,
		dash_vel,
		aoe_cost,
		..
	} = params.abil;
	for (state, mut boost_charge, mut weap_charge, mut vel, player) in &mut q {
		let player = **player;
		if state.just_pressed(&Jump) && *boost_charge >= jump_cost {
			**boost_charge -= *jump_cost;
			jump(
				&mut cmds,
				&mut vel.linvel,
				jump_vel,
				#[cfg(feature = "bevy_kira_audio")]
				&audio,
				#[cfg(feature = "bevy_kira_audio")]
				&sfx,
				&antigrav_q,
				player,
			);
		}

		if state.just_pressed(&Dash) && *boost_charge >= dash_cost {
			**boost_charge -= *dash_cost;
			dash(
				state.clamped_axis_pair(&Move),
				dash_vel,
				&mut vel.linvel,
				#[cfg(feature = "bevy_kira_audio")]
				&audio,
				#[cfg(feature = "bevy_kira_audio")]
				&sfx,
			);
		}

		if state.just_pressed(&FireA) {
			let cam_pivot = cam_pivot_q
				.iter()
				.find_map(|(global, owner)| (**owner == player).then_some(*global))
				.unwrap_or_else(|| panic!("Can't find CamPivot for player {player}"));
			fire_a(
				&mut cmds,
				#[cfg(feature = "bevy_kira_audio")]
				&audio,
				#[cfg(feature = "bevy_kira_audio")]
				&sfx,
				cam_pivot,
				&arm_q,
				&orb_q,
				&chunk_q,
				player,
				&frame,
				&loaded_chunks,
			)
		}

		if state.just_pressed(&AoE) && *weap_charge >= aoe_cost {
			**weap_charge -= *aoe_cost;
			aoe(
				&mut cmds,
				#[cfg(feature = "bevy_kira_audio")]
				&audio,
				#[cfg(feature = "bevy_kira_audio")]
				&sfx,
				&arms_q,
				&arm_q,
				&orb_q,
				player,
			)
		}
	}
}

pub fn jump(
	cmds: &mut Commands,
	linvel: &mut Vec3,
	jump_vel: f32,
	#[cfg(feature = "bevy_kira_audio")] audio: &Audio,
	#[cfg(feature = "bevy_kira_audio")] sfx: &Sfx,
	antigrav_q: &Query<(Entity, &BelongsToPlayer), WithVariant<AntigravParticles>>,
	player: PlayerId,
) {
	#[cfg(feature = "bevy_kira_audio")]
	audio.play(sfx.jump.clone()).with_volume(0.1);
	linvel.z = jump_vel;
	for (id, owner) in antigrav_q {
		if **owner != player {
			continue;
		}
		let mut elapsed = Duration::ZERO;
		let dur = Duration::from_secs_f32(1.5);
		let start = Duration::from_micros(144);
		let end = Duration::from_millis(72);
		cmds.entity(id)
			.start_animation(move |id, _val, t, mut ctrl| {
				elapsed += t.delta();
				if elapsed >= dur {
					elapsed = dur;
					ctrl.end();
				}
				let progress = elapsed.as_secs_f32() / dur.as_secs_f32();
				let new = start + (end.delta_from(&start) * progress);
				ComponentDelta::<Spewer>::new(id, progress, move |mut val, coef| {
					let diff = new.delta_from(&val.interval) * coef;
					if diff != DurationDelta::ZERO {
						val.interval = val.interval + diff;
					}
				})
			});
	}
}

pub fn dash(
	input_dir: Vec2,
	dash_vel: f32,
	linvel: &mut Vec3,
	#[cfg(feature = "bevy_kira_audio")] audio: &Audio,
	#[cfg(feature = "bevy_kira_audio")] sfx: &Sfx,
) {
	#[cfg(feature = "bevy_kira_audio")]
	audio.play(sfx.dash.clone()).with_volume(0.2);
	// Use the most-recently-input direction, not current velocity, to dash in the direction the player expects.
	let dir = input_dir.normalize_or(Vec2::Y) * dash_vel;
	linvel.x = dir.x;
	linvel.y = dir.y;
}

pub fn aoe(
	cmds: &mut Commands,
	#[cfg(feature = "bevy_kira_audio")] audio: &Audio,
	#[cfg(feature = "bevy_kira_audio")] sfx: &Sfx,
	arms_q: &Query<(Entity, &RotVel, &BelongsToPlayer), WithVariant<Arms>>,
	arm_q: &Query<(
		Entity,
		&Transform,
		&GlobalTransform,
		&BelongsToPlayer,
		ERef<Arm>,
	)>,
	orb_q: &Query<(
		Entity,
		&Transform,
		&GlobalTransform,
		&Spewer,
		&BelongsToPlayer,
		ERef<Orb>,
	)>,
	player: PlayerId,
) {
	#[cfg(feature = "bevy_kira_audio")]
	audio.play(sfx.aoe.clone()).with_volume(0.5);
	let extend_dur = Duration::from_millis(64);
	let retract_dur = Duration::from_secs_f32(1.0);
	for (id, rvel, owner) in arms_q {
		if **owner != player {
			continue;
		}
		let quiescent = rvel.quiescent;
		let mut elapsed = Duration::ZERO;
		let start = rvel.quiescent;
		let end = 48.0;
		cmds.entity(id)
			.start_animation(move |target, _val, t, mut ctrl| {
				elapsed += t.delta();
				if elapsed >= extend_dur {
					elapsed = extend_dur;
					ctrl.end();

					let mut elapsed = Duration::ZERO;
					let start = 48.0;
					let end = quiescent;

					ctrl.commands().entity(target).start_animation(
						move |target, _val, t, mut ctrl| {
							elapsed += t.delta();
							if elapsed >= retract_dur {
								elapsed = retract_dur;
								ctrl.end();
							}
							let progress = elapsed.as_secs_f32() / retract_dur.as_secs_f32();
							let new = start.lerp(end, progress.smootherstep());
							ComponentDelta::<RotVel>::mapped(target, progress, new, |val| {
								&mut val.current
							})
						},
					);
				}

				let progress = elapsed.as_secs_f32() / extend_dur.as_secs_f32();
				let new = start.lerp(end, progress.smootherstep());
				ComponentDelta::<RotVel>::mapped(target, progress, new, |val| &mut val.current)
			});
	}

	for (id, xform, _, owner, _) in arm_q {
		if **owner != player {
			continue;
		}

		let start = *xform;
		let end = Transform {
			translation: xform.translation * 6.0,
			scale: Vec3::splat(4.0),
			..*xform
		};
		let mut elapsed = Duration::ZERO;

		cmds.entity(id)
			.start_animation(move |target, _val, t, mut ctrl| {
				elapsed += t.delta();
				if elapsed >= extend_dur {
					elapsed = extend_dur;
					ctrl.end();

					let (start, end) = (end, start);
					let mut elapsed = Duration::ZERO;

					ctrl.commands().entity(target).start_animation(
						move |target, _val, t, mut ctrl| {
							elapsed += t.delta();
							if elapsed >= retract_dur {
								elapsed = retract_dur;
								ctrl.end()
							}

							let progress = elapsed.as_secs_f32() / retract_dur.as_secs_f32();
							let new = start.lerp_slerp(end, progress.smootherstep());
							ComponentDelta::<Transform>::diffable(target, progress, new)
						},
					);
				}

				let progress = elapsed.as_secs_f32() / extend_dur.as_secs_f32();
				let new = start.lerp_slerp(end, progress.smootherstep());
				ComponentDelta::<Transform>::diffable(target, progress, new)
			});
	}

	for (id, _, _, spewer, owner, _) in orb_q {
		if **owner != player {
			continue;
		}

		let dur = extend_dur + retract_dur;
		let mut elapsed = Duration::ZERO;
		let end = spewer.interval;
		let start = Duration::from_micros(10);
		let mut cmds = cmds.entity(id);
		cmds.insert(Hurtbox); // TODO: Should probably be able to add multiple hurtboxes from different animations
		cmds.start_animation(move |id, _val, t, mut ctrl| {
			elapsed += t.delta();
			if elapsed >= dur {
				elapsed = dur;
				ctrl.end();
				ctrl.commands().entity(id).remove::<Hurtbox>();
			}
			let progress = elapsed.as_secs_f32() / dur.as_secs_f32();

			let new = (end - start).mul_f32(progress) + start;
			ComponentDelta::<Spewer>::mapped(id, progress, new, |val| &mut val.interval)
		});
	}
}

pub fn fire_a(
	cmds: &mut Commands,
	#[cfg(feature = "bevy_kira_audio")] audio: &Audio,
	#[cfg(feature = "bevy_kira_audio")] sfx: &Sfx,
	cam_pivot: GlobalTransform,
	arm_q: &Query<(
		Entity,
		&Transform,
		&GlobalTransform,
		&BelongsToPlayer,
		ERef<Arm>,
	)>,
	orb_q: &Query<(
		Entity,
		&Transform,
		&GlobalTransform,
		&Spewer,
		&BelongsToPlayer,
		ERef<Orb>,
	)>,
	chunk_q: &Query<&GlobalTransform, With<ChunkIndex>>,
	player: PlayerId,
	frame: &Frame,
	loaded_chunks: &LoadedChunks,
) {
	#[cfg(feature = "bevy_kira_audio")]
	audio.play(sfx.fire_a.clone());
	for (id, xform, _, _, owner, which) in orb_q {
		if **owner != player || which.0 != PlayerArm::A {
			continue;
		}

		let arm_id = arm_q
			.iter()
			.find_map(|item| (**item.3 == player && item.4 .0 == PlayerArm::A).then_some(item.0))
			.unwrap();

		let mut elapsed = Duration::ZERO;
		let dur = Duration::from_millis(64);
		let start = *xform;
		let end = cam_pivot
			* Transform {
				translation: Vec3::Y * 128.0,
				..default()
			};
		let coords = frame.planet_coords_of(end.translation().xy());
		let Some((_, chunk_id)) = loaded_chunks.closest_to(coords) else {
			error!("No chunks are loaded");
			return;
		};
		let chunk_global = chunk_q.get(chunk_id).unwrap();
		let end_rel = end.reparented_to(chunk_global);
		let mut cmds = cmds.entity(id);
		cmds.insert(Hurtbox);
		cmds.start_animation::<Transform>(move |id, _val, t, mut ctrl| {
			elapsed += t.delta();
			if elapsed >= dur {
				elapsed = dur;
				ctrl.end();
				ctrl.commands().entity(id).remove::<Hurtbox>();
				ctrl.commands().spawn(
					BlendTargets::new(
						id,
						Target::RelativeTo {
							entity: chunk_id,
							relative: end_rel,
						},
						Target::RelativeTo {
							entity: arm_id,
							relative: Transform::IDENTITY,
						},
						Duration::from_millis(256),
					)
					.with_easing(<f32 as Easings>::smootherstep as _),
				);
			}

			let progress = (elapsed.as_secs_f32() / dur.as_secs_f32()).ease_out_cubic();
			let new = start.lerp_slerp(end.compute_transform(), progress); // FIXME: Blend relative to chunk so frame shifting doesn't break it
			ComponentDelta::<Transform>::diffable(id, progress, new)
		});
	}
}

#[derive(
	Component,
	Deref,
	DerefMut,
	Serialize,
	Deserialize,
	Reflect,
	Copy,
	Clone,
	Default,
	Debug,
	PartialEq,
	PartialOrd,
)]
pub struct BoosterCharge(pub f32);

#[derive(
	Component,
	Deref,
	DerefMut,
	Serialize,
	Deserialize,
	Reflect,
	Copy,
	Clone,
	Default,
	Debug,
	PartialEq,
	PartialOrd,
)]
pub struct WeaponCharge(pub f32);

pub fn fill_weapons(mut q: Query<&mut WeaponCharge>, params: Res<PlayerParams>, t: Res<Time>) {
	for mut charge in &mut q {
		if *charge <= params.abil.weapon_capacity {
			**charge = f32::min(
				*params.abil.weapon_capacity,
				**charge + *params.abil.weapon_fill_per_second * t.delta_secs(),
			);
		}
	}
}

#[cfg(feature = "bevy_kira_audio")]
#[derive(Debug, Resource)]
pub struct Sfx {
	pub fire_a: Handle<AudioSource>,
	pub aoe: Handle<AudioSource>,
	pub dash: Handle<AudioSource>,
	pub jump: Handle<AudioSource>,
	pub impacts: [Handle<AudioSource>; 5],
}

#[derive(Component, Debug)]
#[component(storage = "SparseSet")]
pub struct Hurtbox;

#[derive(Component, Default, Debug, Copy, Clone)]
pub struct HurtboxFilter {
	/// Flags indicating what particular type of colliders should be excluded.
	pub flags: QueryFilterFlags,
	/// If set, only colliders with collision groups compatible with this one will
	/// be included in the scene query.
	pub groups: Option<CollisionGroups>,
	/// If set, the collider attached to that entity will be excluded from the query.
	pub exclude_collider: Option<Entity>,
	/// If set, any collider attached to the rigid-body attached to that entity
	/// will be excluded from the query.
	pub exclude_rigid_body: Option<Entity>,
}

impl<'a> From<&'a HurtboxFilter> for QueryFilter<'a> {
	fn from(value: &'a HurtboxFilter) -> Self {
		Self {
			flags: value.flags,
			groups: value.groups,
			exclude_collider: value.exclude_collider,
			exclude_rigid_body: value.exclude_rigid_body,
			predicate: None,
		}
	}
}

#[derive(Event, Copy, Clone, Debug)]
pub struct Hurt {
	pub hurtbox: Entity,
	pub hit: ShapeCastHit,
	pub victim: Entity,
}

pub fn hit_stuff(
	ctx: Single<&RapierContext>,
	q: Query<
		(
			Entity,
			&GlobalTransform,
			&PreviousGlobalTransform,
			&Collider,
			Option<&HurtboxFilter>,
		),
		With<Hurtbox>,
	>,
	mut events: EventWriter<Hurt>,
) {
	for (id, xform, prev, col, filter) in &q {
		let xform = xform.compute_transform();
		let prev = prev.compute_transform();
		let vel = xform.translation - prev.translation;
		// TODO: Get all hits along cast, not just the first.
		// Maybe use `intersections_with_shape` with an extruded shape somehow?
		// Would be easy with spheres (capsule) but not all shapes.
		let Some((other, toi)) = ctx.cast_shape(
			prev.translation,
			prev.rotation.slerp(xform.rotation, 0.5), // Average I guess? *shrugs*
			vel,
			col,
			ShapeCastOptions {
				max_time_of_impact: vel.length() * 1.05,
				stop_at_penetration: false,
				..default()
			},
			filter.map_or_else(QueryFilter::default, |filter| filter.into()),
		) else {
			continue;
		};
		events.send(Hurt {
			hurtbox: id,
			hit: toi,
			victim: other,
		});
	}
}
