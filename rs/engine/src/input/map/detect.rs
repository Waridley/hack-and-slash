use crate::{
	input::{
		map::{
			icons::{Icon, InputIcons},
			widgets::{InputIcon, InputIconBundle},
			GamepadSeries,
		},
		ChordEntry, CurrentChord, InputState,
	},
	ui::{
		layout::LineUpChildren,
		widgets::{
			CuboidFaces, Font3d, Panel, PanelBundle, RectBorderDesc, RectCorners, Text3d,
			Text3dBundle,
		},
		CamAnchor, GlobalUi, TextMeshCache, UiFonts, GLOBAL_UI_RENDER_LAYERS,
	},
};
use bevy::{
	prelude::*,
	utils::{smallvec::SmallVec, HashMap},
};
use bevy_svg::prelude::Origin;

pub struct DetectBindingPopupPlugin;

impl Plugin for DetectBindingPopupPlugin {
	fn build(&self, app: &mut App) {
		app.add_systems(PreUpdate, setup)
			.add_systems(Update, (manage_detect_popup, display_curr_chord))
			.add_systems(Last, InputIcon::sync::<StandardMaterial>);
	}
}

pub fn setup(
	mut events: EventReader<AssetEvent<Font3d>>,
	mut cmds: Commands,
	mut mats: ResMut<Assets<StandardMaterial>>,
	ui_fonts: Res<UiFonts>,
) {
	// TODO: Use bevy_asset_loader
	for event in events.read() {
		if let AssetEvent::Added { id } = event {
			if ui_fonts.mono_3d.id() == *id {
				let size = Vec3::new(8.0, 1.0, 6.0);
				let mut cmds = cmds.spawn((
					PanelBundle {
						data: Panel {
							size,
							borders: CuboidFaces {
								front: vec![RectBorderDesc {
									width: 0.125,
									dilation: 3.0,
									depth: 0.125,
									colors: Some(RectCorners {
										top_right: Color::rgba(0.0, 0.2, 0.2, 0.6),
										top_left: Color::rgba(0.0, 0.4, 0.1, 0.6),
										bottom_left: Color::rgba(0.0, 0.2, 0.2, 0.6),
										bottom_right: Color::rgba(0.0, 0.1, 0.4, 0.6),
									}),
									material: mats.add(StandardMaterial {
										alpha_mode: AlphaMode::Blend,
										..default()
									}),
									..default()
								}],
								..default()
							},
							..default()
						},
						material: mats.add(StandardMaterial {
							base_color: Color::rgba(0.0, 0.001, 0.001, 0.6),
							alpha_mode: AlphaMode::Blend,
							double_sided: true,
							cull_mode: None,
							..default()
						}),
						transform: Transform::from_translation(Vec3::NEG_Y * 10.0),
						visibility: Visibility::Hidden,
						..default()
					},
					DetectBindingPopup,
				));

				cmds.with_children(|cmds| {
					cmds.spawn((Text3dBundle {
						text_3d: Text3d {
							text: "Press input(s) to bind...".into(),
							flat: false,
							vertex_scale: Vec3::new(0.5, 0.5, 0.5),
							..default()
						},
						font: ui_fonts.mono_3d.clone(),
						material: mats.add(Color::AQUAMARINE),
						transform: Transform {
							translation: Vec3::new(0.0, -1.0, 2.0),
							..default()
						},
						..default()
					},));

					cmds.spawn((
						TransformBundle::from_transform(Transform {
							translation: Vec3::new(-0.0, -2.0, -1.0),
							..default()
						}),
						VisibilityBundle::default(),
						CurrChordIcons::default(),
						LineUpChildren {
							relative_positions: Vec3::new(1.5, -1.5, -0.32).normalize(),
							align: Vec3::new(-0.5, -1.0, 0.0),
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
	anchor: Query<Entity, (With<GlobalUi>, With<CamAnchor>)>,
) {
	let Ok((id, parent)) = q.get_single() else {
		return;
	};
	let root = anchor.single();
	if *state.get() == InputState::DetectingBinding {
		if parent.is_none() {
			cmds.entity(id).set_parent(root);
		} else {
			debug_assert_eq!(parent.unwrap().get(), root)
		}
	} else {
		if parent.is_some() {
			debug_assert_eq!(parent.unwrap().get(), root);
			cmds.entity(id).remove_parent();
		}
	}
}

#[derive(Component, Default, Debug, Deref, DerefMut)]
pub struct CurrChordIcons(HashMap<ChordEntry, SmallVec<[Entity; 4]>>);

pub fn display_curr_chord(
	mut cmds: Commands,
	mut q: Query<(Entity, &mut CurrChordIcons)>,
	mut mats: ResMut<Assets<StandardMaterial>>,
	ui_fonts: Res<UiFonts>,
	curr_chord: Res<CurrentChord>,
	gamepads: Res<Gamepads>,
	mut icon_mat: Local<Option<Handle<StandardMaterial>>>,
) {
	let icon_mat = icon_mat.get_or_insert_with(|| mats.add(StandardMaterial::default()));
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
				let entry_icons = InputIcons::from_input_kind(
					entry.0,
					entry
						.1
						.and_then(|gp| gamepads.name(gp))
						.map(GamepadSeries::guess),
				)
				.unwrap_or_default();
				match entry_icons {
					InputIcons::Single(icon) => [icon].into_iter().collect(),
					InputIcons::DualAxis {
						horizontal,
						vertical,
					} => [horizontal, vertical].into_iter().collect(),
				}
			};
			let mut ids = SmallVec::new();
			for icon in entry_icons {
				let icon_id = cmds
					.spawn(InputIconBundle::<StandardMaterial> {
						input_icon: InputIcon {
							icon,
							size: Vec3::splat(1.0),
							..default()
						},
						font: ui_fonts.mono_3d.clone(),
						transform: Transform {
							// scale: Vec3::splat(0.02),
							..default()
						},
						material: icon_mat.clone(),
						..default()
					})
					.id();
				cmds.entity(id).add_child(icon_id);
				ids.push(icon_id);
			}
			icons.insert(*entry, ids);
		}
	}
}
