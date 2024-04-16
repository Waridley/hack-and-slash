use crate::{
	input::{ChordEntry, CurrentChord, InputState},
	ui::{
		in_map::{
			icons::{Icon, InputIcons},
			GamepadSeries,
		},
		layout::ChildrenConstraint,
		widgets::{Font3d, IconWidgetBuilder, PanelBuilder, TextBuilder},
		UiFonts,
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
			.add_systems(Update, (manage_detect_popup_vis, display_curr_chord));
	}
}

pub fn setup(
	mut events: EventReader<AssetEvent<Font3d>>,
	mut cmds: Commands,
	mut meshes: ResMut<Assets<Mesh>>,
	mut mats: ResMut<Assets<StandardMaterial>>,
	mut fonts: ResMut<Assets<Font3d>>,
	ui_fonts: Res<UiFonts>,
) {
	// TODO: Use bevy_asset_loader
	for event in events.read() {
		if let AssetEvent::Added { id } = event {
			if ui_fonts.mono_3d.id() == *id {
				cmds.spawn((
					PanelBuilder {
						size: Vec3::new(16.0, 0.5, 12.0),
						material: mats.add(StandardMaterial {
							base_color: Color::rgba(0.0, 0.2, 0.2, 0.1),
							alpha_mode: AlphaMode::Add,
							double_sided: true,
							cull_mode: None,
							..default()
						}),
						visibility: Visibility::Hidden,
						..default()
					}
					.build(&mut meshes),
					DetectBindingPopup,
				))
				.with_children(|cmds| {
					cmds.spawn((TextBuilder {
						text: "Press input(s) to bind...".into(),
						font: ui_fonts.mono_3d.clone(),
						flat: false,
						material: mats.add(Color::AQUAMARINE),
						transform: Transform {
							translation: Vec3::new(-6.0, 0.0, 4.0),
							rotation: Quat::from_rotation_arc(Vec3::Z, Vec3::NEG_Y),
							..default()
						},
						..default()
					}
					.build(meshes.reborrow(), fonts.reborrow())
					.unwrap(),));

					cmds.spawn((
						TransformBundle::from_transform(Transform {
							translation: Vec3::new(0.0, -0.1, 0.0),
							..default()
						}),
						VisibilityBundle::default(),
						CurrChordIcons::default(),
						ChildrenConstraint {
							alignment: 0.0,
							..default()
						},
					));
				});
			}
		}
	}
}

#[derive(Component, Default, Debug, Reflect)]
pub struct DetectBindingPopup;

pub fn manage_detect_popup_vis(
	state: Res<State<InputState>>,
	mut q: Query<&mut Visibility, With<DetectBindingPopup>>,
) {
	let Ok(mut vis) = q.get_single_mut() else {
		return;
	};
	let new = if *state.get() == InputState::DetectingBinding {
		Visibility::Visible
	} else {
		Visibility::Hidden
	};
	if *vis != new {
		*vis = new;
	}
}

#[derive(Component, Default, Debug, Deref, DerefMut)]
pub struct CurrChordIcons(HashMap<ChordEntry, SmallVec<[Entity; 4]>>);

pub fn display_curr_chord(
	mut cmds: Commands,
	mut q: Query<(Entity, &mut CurrChordIcons)>,
	asset_server: Res<AssetServer>,
	mut meshes: ResMut<Assets<Mesh>>,
	mut fonts: ResMut<Assets<Font3d>>,
	ui_fonts: Res<UiFonts>,
	curr_chord: Res<CurrentChord>,
	gamepads: Res<Gamepads>,
) {
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
				let icon_id = IconWidgetBuilder {
					icon,
					size: Vec2::splat(2.0),
					font: ui_fonts.mono_3d.clone(),
					origin: Origin::Center,
					transform: Transform {
						translation: Vec3::NEG_Y * 0.1,
						scale: Vec3::splat(0.04),
						..default()
					},
					..default()
				}
				.spawn(
					&mut cmds,
					&asset_server,
					meshes.reborrow(),
					fonts.reborrow(),
				)
				.id();
				cmds.entity(id).add_child(icon_id);
				ids.push(icon_id);
			}
			icons.insert(*entry, ids);
		}
	}
}
