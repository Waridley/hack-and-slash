use bevy::{color::palettes::basic::PURPLE, prelude::*};
use engine::{
	entity_tree,
	ui::{
		layout::ExpandToFitChildren,
		text::UiFonts,
		widgets::{new_unlit_material, CuboidPanel, Text3d},
		Fade, MenuStack, UiMat, UiMatBuilder,
	},
};

pub struct PrefsMenuPlugin;

impl Plugin for PrefsMenuPlugin {
	fn build(&self, app: &mut App) {
		app.add_systems(Startup, setup);
	}
}

pub fn setup(mut cmds: Commands, mut mats: ResMut<Assets<UiMat>>, fonts: Res<UiFonts>) {
	entity_tree!(cmds; (
		Name::new("PrefsMenu"),
		PrefsMenu,
		CuboidPanel::default(),
		MeshMaterial3d(mats.add(UiMatBuilder::from(Color::from(PURPLE)))),
		Transform {
			translation: Vec3::new(0.0, -32.0, 24.0),
			..default()
		},
		ExpandToFitChildren {
			offset: Vec3::Y * 0.51,
			..default()
		},
		Fade::ZERO,
		;
		=> |cmds| {
			MenuStack::observe_pop_on_back(cmds, 0.5);
		}
		#children: [
			(
				Text3d {
					text: "Player Preferences".into(),
					font: fonts.mono.clone(),
					..default()
				},
				MeshMaterial3d(mats.add(new_unlit_material())),
			)
		]
	));
}

#[derive(Component, Debug)]
pub struct PrefsMenu;
