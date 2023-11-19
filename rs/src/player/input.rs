use crate::player::player_entity::Arms;
use crate::util::Lerp;
use crate::{
	player::{
		camera::CameraVertSlider,
		ctrl::CtrlVel,
		player_entity::{Arm, CamPivot},
		BelongsToPlayer, RotVel, ACCEL, JUMP_VEL, MAX_JUMPS, MAX_SPEED,
	},
	terminal_velocity,
	// ui::pause_menu::PauseMenuAction,
};
use bevy::{math::Vec3Swizzles, prelude::*};
use bevy_kira_audio::prelude::{Audio, AudioSource, *};
use enum_components::ERef;
use leafwing_abilities::prelude::*;
use leafwing_input_manager::prelude::*;
use particles::Spewer;
use serde::{Deserialize, Serialize};
use std::{
	f32::consts::{PI, TAU},
	time::Duration,
};

pub fn plugin(app: &mut App) -> &mut App {
	app.add_plugins((
		InputManagerPlugin::<PlayerAction>::default(),
		AbilityPlugin::<PlayerAction>::default(),
	))
	.add_systems(First, setup)
	.add_systems(Update, abilities)
	.add_systems(Update, jump.before(terminal_velocity))
}

fn setup(
	mut cmds: Commands,
	q: Query<Entity, (With<ActionState<PlayerAction>>, Without<InputStorageTime>)>,
) {
	for id in q.iter() {
		let duration = Duration::from_millis(120);
		let mut timer = Timer::new(duration, TimerMode::Once);
		timer.set_elapsed(duration);
		cmds.entity(id).insert(InputStorageTime(timer));
	}
}

#[derive(
	Actionlike,
	Abilitylike,
	Copy,
	Clone,
	Debug,
	PartialEq,
	Eq,
	PartialOrd,
	Ord,
	Hash,
	Reflect,
	Serialize,
	Deserialize,
)]
pub enum PlayerAction {
	Jump,
	AoE,
	Pause,
}

impl PlayerAction {
	pub fn cooldown(&self) -> Option<Cooldown> {
		use PlayerAction::*;

		let secs = match *self {
			Jump => 2.0,
			AoE => 2.5,
			Pause => return None,
		};

		Some(Cooldown::from_secs(secs))
	}

	fn cooldowns() -> CooldownState<Self> {
		let mut cooldowns = CooldownState::default();

		for ability in Self::variants() {
			ability.cooldown().map(|cd| cooldowns.set(ability, cd));
		}

		cooldowns
	}

	fn charges() -> ChargeState<Self> {
		use PlayerAction::*;

		ChargeState::default()
			.set(Jump, Charges::simple(MAX_JUMPS as _))
			.build()
	}

	pub fn abilities_bundle() -> AbilitiesBundle<Self> {
		AbilitiesBundle {
			cooldowns: Self::cooldowns(),
			charges: Self::charges(),
		}
	}
}

#[derive(Debug, Resource, Deref, DerefMut)]
pub struct AoESound(pub Handle<AudioSource>);

pub fn abilities(
	mut action_q: Query<AbilityState<PlayerAction>>,
	mut arms_q: Query<&mut RotVel, ERef<Arms>>,
	mut arm_q: Query<(&mut Transform, &mut Spewer), ERef<Arm>>,
	// mut pause_events: EventWriter<PauseMenuAction>,
	sfx: Res<AoESound>,
	audio: Res<Audio>,
	t: Res<Time>,
) {
	use PlayerAction::*;
	for mut state in action_q.iter_mut() {
		match state.trigger_if_just_pressed(AoE) {
			Ok(()) => {
				audio.play(sfx.0.clone()).with_volume(0.5);
				for mut rvel in &mut arms_q {
					**rvel = 36.0;
				}
				for (mut arm, mut spewer) in &mut arm_q {
					// TODO: Filter by player
					arm.translation *= 6.0;
					arm.scale *= 6.0;
					spewer.interval = Duration::from_micros(200);
				}
			}
			Err(CannotUseAbility::OnCooldown) => {
				let cd = state.cooldowns.get(AoE).as_ref().unwrap().remaining();
				info!("Cooldown: {cd:?}");
			}
			_ => {}
		}

		// if state.trigger_if_just_pressed(Pause).is_ok() {
		// 	pause_events.send(PauseMenuAction::ShowOrHide)
		// }

		for mut rvel in &mut arms_q {
			**rvel = **rvel + (rvel.quiescent - **rvel) * t.delta_seconds();
		}
		for (mut arm, mut spewer) in &mut arm_q {
			// TODO: Filter by player
			arm.translation = arm
				.translation
				.lerp(arm.translation.normalize() * 2.0, t.delta_seconds() * 2.0);
			arm.scale = arm.scale.lerp(Vec3::ONE, t.delta_seconds() * 2.0);
			spewer.interval = Duration::from_secs_f32(
				spewer.interval.as_secs_f32().lerp(0.001, t.delta_seconds()),
			);
		}
		// screen_print!("{:#?}", (&state.action_state, &state.charges, &state.cooldowns))
	}
}

#[derive(Component, Resource, Reflect, Default, Debug, Clone)]
struct InputStorageTime(Timer);

fn jump(
	mut q: Query<(
		AbilityState<PlayerAction>,
		&mut CtrlVel,
		&mut InputStorageTime,
	)>,
	t: Res<Time>,
) {
	for (mut state, mut vel, mut storage) in q.iter_mut() {
		storage.0.tick(t.delta());
		let triggered = if storage.0.finished() {
			if state.action_state.just_pressed(PlayerAction::Jump) {
				if state.trigger(PlayerAction::Jump).is_ok() {
					true
				} else {
					storage.0.reset();
					false
				}
			} else {
				false
			}
		} else {
			state.trigger(PlayerAction::Jump).is_ok()
		};

		if triggered {
			vel.linvel.z = JUMP_VEL
		}
	}
}

pub fn look_input(
	mut player_q: Query<(&mut CtrlVel, &BelongsToPlayer)>,
	mut camera_pivot_q: Query<
		(&mut Transform, &mut CameraVertSlider, &BelongsToPlayer),
		ERef<CamPivot>,
	>,
	kb: Res<Input<KeyCode>>,
	t: Res<Time>,
) {
	for (mut vel, player_id) in player_q.iter_mut() {
		let delta = (TAU / 0.5/* seconds to max angvel */) * t.delta_seconds();

		let mut x_input = false;
		if kb.pressed(KeyCode::Left) {
			x_input = true;
			vel.angvel.z = (-TAU).max(vel.angvel.z - delta);
		}
		if kb.pressed(KeyCode::Right) {
			x_input = true;
			vel.angvel.z = TAU.min(vel.angvel.z + delta);
		}
		if !x_input {
			vel.angvel.z *= 0.8
		}

		let (mut xform, mut slider) = camera_pivot_q
			.iter_mut()
			.find_map(|(xform, slider, owner)| (owner == player_id).then_some((xform, slider)))
			.unwrap();
		if kb.pressed(KeyCode::Up) {
			slider.0 = (slider.0 - delta * 0.1).max(0.0);
		}
		if kb.pressed(KeyCode::Down) {
			slider.0 = (slider.0 + delta * 0.1).min(1.0);
		}

		let angle = (-PI) + ((PI * 0.9) * slider.0);
		xform.rotation = xform.rotation.slerp(Quat::from_rotation_x(angle), delta);
	}
}

pub fn movement_input(mut q: Query<&mut CtrlVel>, kb: Res<Input<KeyCode>>, t: Res<Time>) {
	for mut ctrl_vel in &mut q {
		let (mut x, mut y) = (0.0, 0.0);

		let dt = t.delta_seconds();

		if kb.pressed(KeyCode::W) {
			y += 1.0;
		}
		if kb.pressed(KeyCode::A) {
			x -= 1.0;
		}
		if kb.pressed(KeyCode::S) {
			y -= 1.0;
		}
		if kb.pressed(KeyCode::D) {
			x += 1.0;
		}

		let Vec2 { x, y } = ctrl_vel
			.linvel
			.xy()
			.lerp(Vec2 { x, y }.normalize_or_zero() * MAX_SPEED, ACCEL * dt);

		// Only trigger change detection if actually changed
		if ctrl_vel.linvel.x != x {
			ctrl_vel.linvel.x = x
		}
		if ctrl_vel.linvel.y != y {
			ctrl_vel.linvel.y = y
		}
	}
}
