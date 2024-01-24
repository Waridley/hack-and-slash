use crate::{
	player::{
		camera::CameraVertSlider,
		ctrl::CtrlVel,
		player_entity::{Arm, Arms, CamPivot},
		prefs::LookSensitivity,
		BelongsToPlayer, RotVel, ACCEL, JUMP_VEL, MAX_JUMPS, MAX_SPEED,
	},
	terminal_velocity,
	ui::UiHovered,
	util::Lerp,
};
use bevy::{
	input::mouse::MouseMotion,
	math::Vec3Swizzles,
	prelude::{
		GamepadAxisType::{LeftStickX, LeftStickY, RightStickX, RightStickY},
		*,
	},
	window::CursorGrabMode,
};
use bevy_kira_audio::prelude::{Audio, AudioSource, *};
use enum_components::ERef;
use leafwing_abilities::prelude::*;
use leafwing_input_manager::prelude::*;
use particles::Spewer;
use serde::{Deserialize, Serialize};
use std::{
	f32::consts::{FRAC_PI_2, FRAC_PI_3, PI, TAU},
	time::Duration,
};

pub fn plugin(app: &mut App) -> &mut App {
	app.add_plugins((
		InputManagerPlugin::<PlayerAction>::default(),
		AbilityPlugin::<PlayerAction>::default(),
	))
	.add_systems(First, setup)
	.add_systems(
		Update,
		(
			grab_mouse,
			abilities,
			look_input
				.before(terminal_velocity)
				.before(super::ctrl::move_player),
			jump.before(terminal_velocity),
		),
	)
}

fn setup(
	mut cmds: Commands,
	q: Query<Entity, (With<ActionState<PlayerAction>>, Without<InputStorageTimer>)>,
) {
	for id in q.iter() {
		let duration = Duration::from_millis(64);
		let mut timer = Timer::new(duration, TimerMode::Once);
		timer.set_elapsed(duration);
		cmds.entity(id).insert(InputStorageTimer(timer));
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
			Jump => 0.128, // debouncing
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
		for (i, (mut arm, mut spewer)) in arm_q.iter_mut().enumerate() {
			let resting = Quat::from_rotation_z(i as f32 * FRAC_PI_3 * 2.0) * Vec3::X * 2.0;
			// TODO: Filter by player
			arm.translation = arm.translation.lerp(resting, t.delta_seconds() * 2.0);
			arm.scale = arm.scale.lerp(Vec3::ONE, t.delta_seconds() * 2.0);
			spewer.interval = Duration::from_secs_f32(
				spewer.interval.as_secs_f32().lerp(0.001, t.delta_seconds()),
			);
		}
		// screen_print!("{:#?}", (&state.action_state, &state.charges, &state.cooldowns))
	}
}

#[derive(Component, Resource, Reflect, Default, Debug, Clone, Deref, DerefMut)]
pub struct InputStorageTimer(Timer);

pub fn jump(
	mut q: Query<(
		AbilityState<PlayerAction>,
		&mut CtrlVel,
		&mut InputStorageTimer,
	)>,
	t: Res<Time>,
) {
	for (mut state, mut vel, mut storage) in q.iter_mut() {
		storage.tick(t.delta());
		let triggered = if state
			.cooldowns
			.get(PlayerAction::Jump)
			.as_ref()
			.unwrap()
			.ready()
			.is_ok()
		{
			if storage.finished() {
				// No storage timer is set right now
				if state.action_state.just_pressed(PlayerAction::Jump) {
					match state.trigger(PlayerAction::Jump) {
						Ok(()) => true,
						Err(CannotUseAbility::OnCooldown) => false, // Debounce
						Err(CannotUseAbility::NoCharges) => {
							// Couldn't trigger right now, keep trying over next few frames
							storage.reset();
							false
						}
						Err(e) => {
							bevy::log::error!("Not sure how to handle {e:?}");
							false
						}
					}
				} else {
					false
				}
			} else {
				// Keep trying until storage timer runs out
				state.trigger(PlayerAction::Jump).is_ok()
			}
		} else {
			false
		};

		if triggered {
			let dur = storage.duration();
			storage.tick(dur); // tick makes sure it finishes, unlike set_elapsed
				   // FIXME: Debouncing isn't working for some reason
			let cd_triggered = state
				.cooldowns
				.get_mut(PlayerAction::Jump)
				.as_mut()
				.unwrap()
				.trigger()
				.is_ok();
			if !cd_triggered {
				bevy::log::error!("How did we trigger Jump if the cooldown hadn't elapsed???");
			}
			vel.linvel.z = JUMP_VEL
		}
	}
}

pub fn look_input(
	mut player_q: Query<(&mut CtrlVel, &BelongsToPlayer, &LookSensitivity)>,
	mut camera_pivot_q: Query<
		(&mut Transform, &mut CameraVertSlider, &BelongsToPlayer),
		ERef<CamPivot>,
	>,
	kb: Res<Input<KeyCode>>,
	mut mouse: EventReader<MouseMotion>,
	gp: Res<Gamepads>,
	axes: Res<Axis<GamepadAxis>>,
	windows: Query<&Window>,
	t: Res<Time>,
) {
	for (mut vel, player_id, sens) in player_q.iter_mut() {
		let delta = (TAU / 0.5/* seconds to max angvel */) * t.delta_seconds();

		let mouse = if windows.single().cursor.grab_mode == CursorGrabMode::Locked {
			mouse
				.read()
				.fold(Vec2::ZERO, |acc, delta| acc + delta.delta * **sens)
		} else {
			Vec2::ZERO
		};

		let gp = gp.iter().fold(Vec2::ZERO, |Vec2 { x, y }, gamepad| {
			Vec2::new(
				x + axes
					.get(GamepadAxis {
						gamepad,
						axis_type: RightStickX,
					})
					.unwrap_or_default(),
				y + axes
					.get(GamepadAxis {
						gamepad,
						axis_type: RightStickY,
					})
					.unwrap_or_default(),
			)
		});
		let mut x_input = mouse.x.abs() > 0.0;
		if gp.x.abs() > 0.2 {
			x_input = true;
			vel.angvel.z = (gp.x - (gp.x.signum() * 0.2)) * 1.25 * TAU;
		}
		if kb.pressed(KeyCode::Left) {
			x_input = true;
			vel.angvel.z = (-TAU).max(vel.angvel.z - delta);
		}
		if kb.pressed(KeyCode::Right) {
			x_input = true;
			vel.angvel.z = TAU.min(vel.angvel.z + delta);
		}

		if x_input {
			vel.angvel.z += mouse.x;
		} else {
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
		if gp.y.abs() > 0.2 {
			let y = -gp.y + (0.2 * gp.y.signum()) * 1.25;
			slider.0 = (slider.0 + y * delta * 0.1).clamp(0.0, 1.0);
		}
		if mouse.y.abs() > 0.0 {
			slider.0 = (slider.0 + mouse.y * 0.1).clamp(0.0, 1.0);
		}

		let angle = (-FRAC_PI_2).lerp(FRAC_PI_2 * 0.9, slider.0);
		xform.rotation = xform.rotation.slerp(Quat::from_rotation_x(angle), delta);
	}
}

pub fn movement_input(
	mut q: Query<&mut CtrlVel>,
	kb: Res<Input<KeyCode>>,
	gp: Res<Gamepads>,
	axes: Res<Axis<GamepadAxis>>,
	t: Res<Time>,
) {
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

		for gamepad in gp.iter() {
			if let Some(val) = axes.get(GamepadAxis {
				gamepad,
				axis_type: LeftStickX,
			}) {
				x += f32::max(val.abs() - 0.2, 0.0) * 1.25 * val.signum();
			}
			if let Some(val) = axes.get(GamepadAxis {
				gamepad,
				axis_type: LeftStickY,
			}) {
				y += f32::max(val.abs() - 0.2, 0.0) * 1.25 * val.signum()
			}
		}

		let Vec2 { x, y } = ctrl_vel
			.linvel
			.xy()
			.lerp(Vec2 { x, y }.clamp_length_max(1.0) * MAX_SPEED, ACCEL * dt);

		// Only trigger change detection if actually changed
		if ctrl_vel.linvel.x != x {
			ctrl_vel.linvel.x = x
		}
		if ctrl_vel.linvel.y != y {
			ctrl_vel.linvel.y = y
		}
	}
}
pub fn grab_mouse(
	mut windows: Query<&mut Window>,
	mouse: Res<Input<MouseButton>>,
	key: Res<Input<KeyCode>>,
	ui_hovered: Res<UiHovered>,
) {
	let Ok(mut window) = windows.get_single_mut() else {
		// probably exiting if window is missing
		return;
	};

	if mouse.just_pressed(MouseButton::Left) && !**ui_hovered {
		window.cursor.visible = false;
		window.cursor.grab_mode = CursorGrabMode::Locked;
	}

	if key.just_pressed(KeyCode::Escape) {
		window.cursor.visible = true;
		window.cursor.grab_mode = CursorGrabMode::None;
	}
}
