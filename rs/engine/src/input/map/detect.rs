use crate::{
	draw::PlanarPolyLine,
	input::{
		map::{
			icons::{Icon, InputIconFileMap, UserInputIcons},
			widgets::{InputIcon, InputIconBundle},
			Platform,
		},
		ChordEntry, CurrentChord, InputState,
		InputState::DetectingBinding,
		ToBind,
	},
	mats::{
		fade::DitherFade,
		fog::{DistanceDither, Matter},
		ExtMat,
	},
	ui::{
		layout::LineUpChildren,
		text::UiFonts,
		widgets::{CuboidPanel, Node3d, Text3d},
		GlobalUi, Popup, PopupsRoot, UiMat, UiMatBuilder, GLOBAL_UI_RENDER_LAYERS,
	},
	util::MeshOutline,
};
use bevy::{
	color::palettes::css::AQUAMARINE,
	input::{
		gamepad::{GamepadAxisChangedEvent, GamepadButtonChangedEvent},
		keyboard::KeyboardInput,
		mouse::{MouseButtonInput, MouseMotion, MouseWheel},
	},
	prelude::*,
	utils::HashMap,
};
use leafwing_input_manager::prelude::{MouseMoveDirection, MouseScrollDirection, UserInputWrapper};
use smallvec::{smallvec, SmallVec};

pub struct DetectBindingPopupPlugin;

impl Plugin for DetectBindingPopupPlugin {
	fn build(&self, app: &mut App) {
		app.add_systems(PreUpdate, setup)
			.add_systems(
				Update,
				(
					manage_detect_popup,
					display_curr_chord.run_if(resource_exists::<InputIconFileMap>),
				),
			)
			.add_systems(
				Last,
				InputIcon::sync::<UiMat>
					.before(Text3d::sync_mesh)
					.before(bevy_svg::prelude::svg_mesh_3d_generator)
					.before(bevy_svg::prelude::svg_mesh_2d_generator),
			)
			.configure_sets(
				Last,
				bevy_svg::prelude::Set::SVG.before(crate::util::MeshOutline::sync),
			);
	}
}

pub fn setup(
	mut events: EventReader<AssetEvent<Font>>,
	mut cmds: Commands,
	mut meshes: ResMut<Assets<Mesh>>,
	mut mats: ResMut<Assets<ExtMat<DitherFade>>>,
	ui_fonts: Res<UiFonts>,
) {
	// TODO: Use bevy_asset_loader
	for event in events.read() {
		if let AssetEvent::Added { id } = event {
			if ui_fonts.mono.id() == *id {
				let size = Vec3::new(8.0, 1.0, 6.0);
				let mut cmds = cmds.spawn((
					Popup,
					CuboidPanel { size, ..default() },
					MeshMaterial3d(mats.add(ExtMat {
						extension: default(),
						base: Matter {
							extension: DistanceDither::ui(),
							base: StandardMaterial {
								base_color: Color::srgba(0.0, 0.001, 0.001, 0.6),
								alpha_mode: AlphaMode::Blend,
								double_sided: true,
								cull_mode: None,
								..default()
							},
						},
					})),
					Visibility::Hidden,
					DetectBindingPopup,
				));

				cmds.with_children(|cmds| {
					cmds.spawn((
						Node3d,
						Transform {
							translation: Vec3::NEG_Y * 0.5,
							..default()
						},
						Mesh3d(
							meshes.add(
								PlanarPolyLine {
									colors: smallvec![
										smallvec![Srgba::new(0.0, 0.2, 0.2, 0.6)],
										smallvec![Srgba::new(0.0, 0.4, 0.1, 0.6)],
										smallvec![Srgba::new(0.0, 0.2, 0.2, 0.6)],
										smallvec![Srgba::new(0.0, 0.1, 0.4, 0.6)],
									],
									..PlanarPolyLine::rect(8.0, 6.0, 0.25)
								}
								.mesh()
								.build()
								.with_duplicated_vertices()
								.with_computed_flat_normals(),
							),
						),
						MeshMaterial3d(mats.add(ExtMat {
							extension: default(),
							base: Matter {
								extension: DistanceDither::ui(),
								base: StandardMaterial {
									alpha_mode: AlphaMode::Blend,
									..default()
								},
							},
						})),
					));
					cmds.spawn((
						Text3d {
							text: "Press input(s) to bind...".into(),
							font: ui_fonts.mono.clone(),
							flat: false,
							vertex_scale: Vec3::splat(0.45),
							..default()
						},
						MeshMaterial3d(mats.add(ExtMat {
							extension: default(),
							base: Matter {
								extension: DistanceDither::ui(),
								base: Color::from(AQUAMARINE).into(),
							},
						})),
						Transform {
							translation: Vec3::new(0.0, -1.0, 2.0),
							..default()
						},
					));

					cmds.spawn((
						Transform {
							translation: Vec3::new(-0.0, -2.0, -1.0),
							..default()
						},
						Visibility::default(),
						CurrChordIcons::default(),
						LineUpChildren {
							relative_positions: Vec3::new(1.5, -1.5, -0.32).normalize(),
							align: Vec3::new(-0.5, -1.0, 0.0),
							..default()
						},
						GLOBAL_UI_RENDER_LAYERS,
					));
				});
			}
		}
	}
}

#[derive(Component, Default, Debug, Reflect)]
pub struct DetectBindingPopup;

pub fn manage_detect_popup(
	mut cmds: Commands,
	state: Res<State<InputState>>,
	q: Query<(Entity, Option<&Parent>), With<DetectBindingPopup>>,
	// Popups always show right in front of camera
	anchor: Query<Entity, (With<GlobalUi>, With<PopupsRoot>)>,
) {
	let Ok((id, parent)) = q.get_single() else {
		return;
	};
	let root = anchor.single();
	if *state.get() == DetectingBinding {
		if parent.is_none() {
			cmds.entity(id).set_parent(root);
		} else {
			debug_assert_eq!(parent.unwrap().get(), root)
		}
	} else if parent.is_some() {
		debug_assert_eq!(parent.unwrap().get(), root);
		cmds.entity(id).remove_parent();
	}
}

#[derive(Component, Default, Debug, Deref, DerefMut)]
pub struct CurrChordIcons(HashMap<ChordEntry, SmallVec<[Entity; 4]>>);

pub fn display_curr_chord(
	mut cmds: Commands,
	mut q: Query<(Entity, &mut CurrChordIcons)>,
	mut mats: ResMut<Assets<UiMat>>,
	ui_fonts: Res<UiFonts>,
	curr_chord: Res<CurrentChord>,
	gamepads: Query<(Option<&Name>, &Gamepad)>,
	mut icon_mat: Local<Option<Handle<UiMat>>>,
	icon_map: Res<InputIconFileMap>,
) {
	let icon_mat = icon_mat.get_or_insert_with(|| mats.add(UiMatBuilder::default()));
	let outline_mat = mats.add(MeshOutline::default_material());
	let Ok((id, mut icons)) = q.get_single_mut() else {
		return;
	};

	icons.retain(|entry, ids| {
		let keep = curr_chord.contains_key(entry);
		if !keep {
			for id in ids.iter() {
				cmds.entity(*id).despawn_recursive()
			}
		}
		keep
	});

	for (entry, key) in curr_chord.iter() {
		if !icons.contains_key(entry) {
			let entry_icons: SmallVec<[Icon; 2]> = if let Some(key) = key {
				[super::icons::key(key).unwrap_or_default()]
					.into_iter()
					.collect()
			} else {
				let entry_icons = UserInputIcons::from_user_input(
					&entry.0,
					entry
						.1
						.and_then(|gp| gamepads.get(gp).ok())
						.and_then(|(name, _)| name)
						.map(Name::as_str)
						.and_then(Platform::guess_gamepad),
					&icon_map,
				);

				entry_icons.iter().cloned().collect()
			};
			let mut ids = SmallVec::new();
			for icon in entry_icons {
				let icon_id = cmds
					.spawn(InputIconBundle::<UiMat> {
						input_icon: InputIcon {
							icon,
							font: ui_fonts.mono.clone(),
							size: Vec3::splat(1.0),
							flat: false,
							tolerance: 0.1,
							outline: Some(MeshOutline {
								thickness: Vec3::new(0.1, 0.0, 0.1),
								offset: Vec3::Y * 0.1,
								..default()
							}),
							outline_material: outline_mat.clone().untyped(),
							..default()
						},
						material: MeshMaterial3d(icon_mat.clone()),
						..default()
					})
					.id();
				cmds.entity(id).add_child(icon_id);
				ids.push(icon_id);
			}
			icons.insert(entry.clone(), ids);
		}
	}
}

pub fn detect_bindings(
	mut gamepad_buttons: ResMut<Events<GamepadButtonChangedEvent>>,
	mut gamepad_axes: ResMut<Events<GamepadAxisChangedEvent>>,
	mut keys: ResMut<Events<KeyboardInput>>,
	mut mouse_buttons: ResMut<Events<MouseButtonInput>>,
	mut mouse_wheel: ResMut<Events<MouseWheel>>,
	mut mouse_motion: ResMut<Events<MouseMotion>>,
	mut tx: EventWriter<ToBind>,
	mut curr_chord: ResMut<CurrentChord>,
	mut mouse_accum: Local<Vec2>,
	mut wheel_accum: Local<Vec2>,
) {
	let mut finalize = |chord: &mut CurrentChord, m: &mut Vec2, w: &mut Vec2| {
		*m = Vec2::ZERO;
		*w = Vec2::ZERO;
		let binding = ToBind(std::mem::take(chord));
		info!("{binding:?}");
		tx.send(binding);
	};

	// Only finalize binding when the user *releases* at least one button.
	// This allows chords to be bound, whether physically input by the user,
	// or automatically sent by macro keys for example.

	for ev in gamepad_buttons.drain() {
		let entry = (
			UserInputWrapper::Button(Box::new(ev.button)).into(),
			Some(ev.entity),
		);
		trace!("{entry:?}");
		if ev.state.is_pressed() {
			curr_chord.insert(entry, None);
		} else if curr_chord.contains_key(&entry) {
			finalize(&mut curr_chord, &mut mouse_accum, &mut wheel_accum);
		}
	}

	// Thresholds are higher than usual to make sure the user really wants the given input.

	for axis in gamepad_axes.drain() {
		let entry = (
			UserInputWrapper::Axis(Box::new(axis.axis)).into(),
			Some(axis.entity),
		);
		trace!("{entry:?}");
		if axis.value.abs() > 0.7 {
			curr_chord.insert(entry, None);
		} else if curr_chord.contains_key(&entry) {
			finalize(&mut curr_chord, &mut mouse_accum, &mut wheel_accum);
		}
	}

	for key in keys.drain() {
		let entry = (
			UserInputWrapper::Button(Box::new(key.key_code)).into(),
			None,
		);
		trace!("{entry:?}");
		if key.state.is_pressed() {
			curr_chord.insert(entry, Some(key.logical_key));
		} else if curr_chord.contains_key(&entry) {
			finalize(&mut curr_chord, &mut mouse_accum, &mut wheel_accum);
		}
	}

	for btn in mouse_buttons.drain() {
		let entry = (UserInputWrapper::Button(Box::new(btn.button)).into(), None);
		trace!("{entry:?}");
		if btn.state.is_pressed() {
			curr_chord.insert(entry, None);
		} else if curr_chord.contains_key(&entry) {
			finalize(&mut curr_chord, &mut mouse_accum, &mut wheel_accum);
		}
	}

	for wheel in mouse_wheel.drain() {
		wheel_accum.x += wheel.x;
		wheel_accum.y += wheel.y;
		trace!("{wheel_accum:?}");
		if wheel_accum.x >= 3.0 {
			curr_chord.insert(
				(
					UserInputWrapper::Button(Box::new(MouseScrollDirection::RIGHT)).into(),
					None,
				),
				None,
			);
			finalize(&mut curr_chord, &mut mouse_accum, &mut wheel_accum);
		} else if wheel_accum.x <= -3.0 {
			curr_chord.insert(
				(
					UserInputWrapper::Button(Box::new(MouseScrollDirection::LEFT)).into(),
					None,
				),
				None,
			);
			finalize(&mut curr_chord, &mut mouse_accum, &mut wheel_accum);
		}
		if wheel_accum.y >= 3.0 {
			curr_chord.insert(
				(
					UserInputWrapper::Button(Box::new(MouseScrollDirection::UP)).into(),
					None,
				),
				None,
			);
			finalize(&mut curr_chord, &mut mouse_accum, &mut wheel_accum);
		} else if wheel_accum.y <= -3.0 {
			curr_chord.insert(
				(
					UserInputWrapper::Button(Box::new(MouseScrollDirection::DOWN)).into(),
					None,
				),
				None,
			);
			finalize(&mut curr_chord, &mut mouse_accum, &mut wheel_accum);
		}
	}

	for motion in mouse_motion.drain() {
		*mouse_accum += motion.delta;
		trace!("{mouse_accum:?}");
		// FIXME: Probably scale by sensitivity
		if mouse_accum.x > 500.0 {
			curr_chord.insert(
				(
					UserInputWrapper::Button(Box::new(MouseMoveDirection::RIGHT)).into(),
					None,
				),
				None,
			);
			finalize(&mut curr_chord, &mut mouse_accum, &mut wheel_accum);
		} else if mouse_accum.x < -500.0 {
			curr_chord.insert(
				(
					UserInputWrapper::Button(Box::new(MouseMoveDirection::LEFT)).into(),
					None,
				),
				None,
			);
			finalize(&mut curr_chord, &mut mouse_accum, &mut wheel_accum);
		}
		if mouse_accum.y > 500.0 {
			curr_chord.insert(
				(
					UserInputWrapper::Button(Box::new(MouseMoveDirection::DOWN)).into(),
					None,
				),
				None,
			);
			finalize(&mut curr_chord, &mut mouse_accum, &mut wheel_accum);
		} else if mouse_accum.y < -500.0 {
			curr_chord.insert(
				(
					UserInputWrapper::Button(Box::new(MouseMoveDirection::UP)).into(),
					None,
				),
				None,
			);
			finalize(&mut curr_chord, &mut mouse_accum, &mut wheel_accum);
		}
	}
}

#[cfg(feature = "debugging")]
pub fn dbg_set_detect_binding_state(
	mut stack: ResMut<crate::util::StateStack<InputState>>,
	mut keys: EventReader<KeyboardInput>,
	mut curr_chord: ResMut<CurrentChord>,
) {
	for key in keys.read() {
		if key.key_code == KeyCode::KeyB && key.state == bevy::input::ButtonState::Released {
			match stack.last() {
				Some(DetectingBinding) => {
					stack.pop();
				}
				Some(_) => stack.push(DetectingBinding),
				None => {
					error!("InputState stack was somehow empty.");
					stack.push(InputState::default());
				}
			}
			curr_chord.clear()
		}
	}
}

#[cfg(feature = "debugging")]
pub fn dbg_detect_bindings(
	mut rx: EventReader<ToBind>,
	gamepads: Query<(Option<&Name>, &Gamepad)>,
	icon_map: Res<InputIconFileMap>,
) {
	use crate::input::map::{icons::*, Platform};
	for event in rx.read() {
		for ((input, gp), logical_key) in event.0.iter() {
			let icons = if let Some(icon) = logical_key.as_ref() {
				key(icon).map(UserInputIcons::simple).unwrap_or_default()
			} else {
				UserInputIcons::from_user_input(
					input,
					gp.and_then(|gp| gamepads.get(gp).ok())
						.and_then(|(name, _)| name)
						.map(Name::as_str)
						.and_then(Platform::guess_gamepad),
					&icon_map,
				)
			};

			info!(input = ?input, gamepad = ?gp, icons = ?icons);
		}
	}
}
