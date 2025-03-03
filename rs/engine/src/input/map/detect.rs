use crate::{
	draw::PlanarPolyLine,
	input::{
		map::{
			icons::Icon,
			widgets::{InputIcon, InputIconBundle},
			Platform,
		},
		ChordEntry, CurrentChord, InputState,
	},
	mats::{
		fade::DitherFade,
		fog::{DistanceDither, Matter},
		ExtMat,
	},
	ui::{
		layout::LineUpChildren,
		text::UiFonts,
		widgets::{
			CuboidPanel, CuboidPanelBundle, Text3d,
			Text3dBundle,
		}, GlobalUi, Popup, PopupsRoot, UiMat, UiMatBuilder, GLOBAL_UI_RENDER_LAYERS,
	},
};
use bevy::{
	prelude::*,
	utils::HashMap,
};
use bevy::color::palettes::css::AQUAMARINE;
use smallvec::{smallvec, SmallVec};
use crate::input::map::icons::{InputIconFileMap, UserInputIcons};
use crate::ui::widgets::Node3d;

pub struct DetectBindingPopupPlugin;

impl Plugin for DetectBindingPopupPlugin {
	fn build(&self, app: &mut App) {
		app.add_systems(PreUpdate, setup)
			.add_systems(Update, (manage_detect_popup, display_curr_chord.run_if(resource_exists::<InputIconFileMap>)))
			.add_systems(Last, InputIcon::sync::<UiMat>);
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
					CuboidPanelBundle {
						panel: CuboidPanel { size, ..default() },
						material: MeshMaterial3d(mats.add(ExtMat {
							extension: default(),
							base: Matter {
								extension: DistanceDither::ui(),
								base: StandardMaterial {
									base_color: Color::linear_rgba(0.0, 0.001, 0.001, 0.6),
									alpha_mode: AlphaMode::Blend,
									double_sided: true,
									cull_mode: None,
									..default()
								},
							},
						})),
						..default()
					},
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
						Mesh3d(meshes.add(
							PlanarPolyLine {
								colors: smallvec![
									smallvec![LinearRgba::new(0.0, 0.2, 0.2, 0.6)],
									smallvec![LinearRgba::new(0.0, 0.4, 0.1, 0.6)],
									smallvec![LinearRgba::new(0.0, 0.2, 0.2, 0.6)],
									smallvec![LinearRgba::new(0.0, 0.1, 0.4, 0.6)],
								],
								..PlanarPolyLine::rect(8.0, 6.0, 0.25)
							}
							.mesh()
							.build()
							.with_duplicated_vertices()
							.with_computed_flat_normals(),
						)),
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
						Text3dBundle {
							text_3d: Text3d {
								text: "Press input(s) to bind...".into(),
								font: ui_fonts.mono.clone(),
								flat: false,
								vertex_scale: Vec3::splat(0.45),
								..default()
							},
							material: MeshMaterial3d(mats.add(ExtMat {
								extension: default(),
								base: Matter {
									extension: DistanceDither::ui(),
									base: Color::from(AQUAMARINE).into(),
								},
							})),
							..default()
						},
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
	mut mats: ResMut<Assets<UiMat>>,
	ui_fonts: Res<UiFonts>,
	curr_chord: Res<CurrentChord>,
	gamepads: Query<(Option<&Name>, &Gamepad)>,
	mut icon_mat: Local<Option<Handle<UiMat>>>,
	icon_map: Res<InputIconFileMap>,
) {
	let icon_mat = icon_mat.get_or_insert_with(|| mats.add(UiMatBuilder::default()));
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
					entry.0.clone().into(),
					entry.1
						.and_then(|gp| gamepads.get(gp).ok())
						.and_then(|(name, _)| name)
						.map(Name::as_str)
						.and_then(Platform::guess_gamepad),
					&*icon_map,
				);
				
				
				entry_icons.into_iter().collect()
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
