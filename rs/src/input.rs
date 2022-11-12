use crate::terminal_velocity;
use bevy::prelude::*;
use leafwing_abilities::{AbilitiesBundle, Abilitylike, cooldown::Cooldown, prelude::*};
use leafwing_input_manager::prelude::*;
use std::time::Duration;
use bevy_rapier3d::control::KinematicCharacterControllerOutput;
use std::f32::consts::{PI, TAU};
use bevy_rapier3d::math::Vect;
use crate::player::{ACCEL, CameraVertSlider, MAX_SPEED};
use crate::player::player_entity::{CamPivot, ReadPlayerEntity};

pub struct InputPlugin;

impl Plugin for InputPlugin {
	fn build(&self, app: &mut App) {
		app.add_plugin(InputManagerPlugin::<PlayerAction>::default())
			.add_plugin(AbilityPlugin::<PlayerAction>::default())
			.add_system_to_stage(CoreStage::First, setup)
			.add_system(abilities)
			.add_system(jump.before(terminal_velocity));
	}
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

#[derive(Actionlike, Abilitylike, Copy, Clone, Debug, Reflect, FromReflect)]
pub enum PlayerAction {
	Jump,
}

impl PlayerAction {
	pub fn input_map() -> InputMap<Self> {
		use PlayerAction::*;

		InputMap::new([(KeyCode::Back, Jump), (KeyCode::Space, Jump)])
	}

	pub fn cooldown(&self) -> Cooldown {
		use PlayerAction::*;

		let secs = match *self {
			Jump => 2.0,
		};

		Cooldown::from_secs(secs)
	}

	fn cooldowns() -> CooldownState<Self> {
		let mut cooldowns = CooldownState::default();

		for ability in Self::variants() {
			cooldowns.set(ability, ability.cooldown());
		}

		cooldowns
	}

	fn charges() -> ChargeState<Self> {
		use PlayerAction::*;

		ChargeState::default().set(Jump, Charges::simple(2)).build()
	}

	pub fn abilities_bundle() -> AbilitiesBundle<Self> {
		AbilitiesBundle {
			cooldowns: Self::cooldowns(),
			charges: Self::charges(),
			..default()
		}
	}
}

pub fn abilities(q: Query<AbilityState<PlayerAction>>) {
	for _state in q.iter() {
		// screen_print!("{:#?}", (&state.action_state, &state.charges, &state.cooldowns))
	}
}

#[derive(Component, Resource, Reflect, Default, Debug, Clone)]
struct InputStorageTime(Timer);

fn jump(
	mut q: Query<(
		AbilityState<PlayerAction>,
		&mut InputVel,
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
			vel.linvel.z = 32.0
		}
	}
}

pub fn look_input(
	mut player_q: Query<(&mut InputVel, Option<&KinematicCharacterControllerOutput>)>,
	mut camera_pivot_q: Query<(&mut Transform, &mut CameraVertSlider), ReadPlayerEntity<CamPivot>>,
	kb: Res<Input<KeyCode>>,
	t: Res<Time>,
) {
	let (mut vel, _out) = player_q.single_mut();
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

	let (mut xform, mut slider) = camera_pivot_q.single_mut();
	if kb.pressed(KeyCode::Up) {
		slider.0 = (slider.0 - delta * 0.1).max(0.0);
	}
	if kb.pressed(KeyCode::Down) {
		slider.0 = (slider.0 + delta * 0.1).min(1.0);
	}

	let angle = (-PI) + ((PI * 0.9) * slider.0);
	xform.rotation = xform.rotation.slerp(Quat::from_rotation_x(angle), delta);
}

#[derive(Component, Debug, Default, Copy, Clone, Reflect)]
pub struct InputVel {
	pub linvel: Vect,
	pub angvel: Vect,
}

pub fn movement_input(
	mut q: Query<(&mut InputVel, Option<&KinematicCharacterControllerOutput>)>,
	kb: Res<Input<KeyCode>>,
	t: Res<Time>,
) {
	let (mut input_vel, _out) = q.single_mut();
	// screen_print!("{:#?}", &*input_vel);

	let mut x_input = false;
	let mut y_input = false;
	let Vect { mut x, mut y, .. } = input_vel.linvel; // Not mutably reborrowing to avoid change detection

	let dt = t.delta_seconds();
	
	if kb.pressed(KeyCode::W) {
		y = MAX_SPEED.min(y + ACCEL * dt);
		y_input = true
	}
	if kb.pressed(KeyCode::A) {
		x = (-MAX_SPEED).max(x - ACCEL * dt);
		x_input = true
	}
	if kb.pressed(KeyCode::S) {
		y = (-MAX_SPEED).max(y - ACCEL * dt);
		y_input = true
	}
	if kb.pressed(KeyCode::D) {
		x = MAX_SPEED.min(x + ACCEL * dt);
		x_input = true
	}

	let decel = |v: &mut f32| if *v > 0.0 {
		*v = f32::max(*v - (ACCEL * dt), 0.0)
	} else if *v < 0.0 {
		*v = f32::min(*v + (ACCEL * dt), 0.0)
	};
	
	if !x_input {
		decel(&mut x)
	}
	if !y_input {
		decel(&mut y)
	}
	
	// Only trigger change detection if actually changed
	if input_vel.linvel.x != x {
		input_vel.linvel.x = x
	}
	if input_vel.linvel.y != y {
		input_vel.linvel.y = y
	}
}
