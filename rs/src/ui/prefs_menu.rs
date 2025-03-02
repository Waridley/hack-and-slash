use bevy::color::palettes::basic::PURPLE;
use bevy::prelude::*;
use engine::entity_tree;
use engine::ui::{Fade, MenuStack, UiMat, UiMatBuilder, GLOBAL_UI_RENDER_LAYERS};
use engine::ui::layout::ExpandToFitChildren;
use engine::ui::text::UiFonts;
use engine::ui::widgets::{new_unlit_material, CuboidPanelBundle, Text3d, Text3dBundle};

pub struct PrefsMenuPlugin;

impl Plugin for PrefsMenuPlugin {
	fn build(&self, app: &mut App) {
		app.add_systems(Startup, setup);
	}
}

pub fn setup(
	mut cmds: Commands,
	mut mats: ResMut<Assets<UiMat>>,
	fonts: Res<UiFonts>,
) {
	
	entity_tree!(cmds; (
		Name::new("PrefsMenu"),
		PrefsMenu,
		CuboidPanelBundle {
			transform: Transform {
				translation: Vec3::new(0.0, -32.0, 24.0),
				..default()
			},
			material: MeshMaterial3d(mats.add(UiMatBuilder::from(Color::from(PURPLE)))),
			handlers: MenuStack::pop_on_back(GLOBAL_UI_RENDER_LAYERS, 0.7),
			..default()
		},
		ExpandToFitChildren {
			offset: Vec3::Y * 0.51,
			..default()
		},
		Fade::ZERO,
		;
		#children: [
			(
				Text3dBundle {
					text_3d: Text3d {
						text: "Player Preferences".into(),
						font: fonts.mono.clone(),
						..default()
					},
					material: MeshMaterial3d(mats.add(new_unlit_material())),
					..default()
				}
			)
		]
	));
}

#[derive(Component, Debug)]
pub struct PrefsMenu;

