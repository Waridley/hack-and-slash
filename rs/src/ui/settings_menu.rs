use bevy::prelude::*;
use engine::{
	entity_tree,
	ui::{
		widgets::{
			new_unlit_material, CuboidPanel, CuboidPanelBundle, CylinderFaces, CylinderPanel,
			CylinderPanelBundle, Node3dBundle, Text3d, Text3dBundle,
		},
		Fade, UiFonts, UiMat, UiMatBuilder,
	},
};
use enum_components::{EntityEnumCommands, EnumComponent};

pub struct SettingsMenuPlugin;

impl Plugin for SettingsMenuPlugin {
	fn build(&self, app: &mut App) {
		app.add_systems(Startup, setup);
	}
}

pub fn setup(mut cmds: Commands, mut mats: ResMut<Assets<UiMat>>, ui_fonts: Res<UiFonts>) {
	let border_mat = mats.add(UiMatBuilder::from(Color::DARK_GRAY));
	entity_tree!(cmds; (
		Name::new("SettingsMenu"),
		SettingsMenu,
		CylinderPanelBundle {
			panel: CylinderPanel {
				radius: 24.0,
				length: 18.0,
				..default()
			},
			material: mats.add(UiMatBuilder::from(StandardMaterial {
				base_color: Color::rgba(0.1, 0.1, 0.1, 0.8),
				alpha_mode: AlphaMode::Blend,
				double_sided: true,
				cull_mode: None,
				..default()
			})),
			transform: Transform {
				translation: Vec3::new(0.0, -32.0, -24.0),
				..default()
			},
			..default()
		},
		Fade::ZERO;
		#children:
			(
				Node3dBundle {
					transform: Transform {
						translation: Vec3::NEG_Y * 25.0,
						..default()
					},
					..default()
				},
				=> |cmds| {
					cmds.set_enum(settings_sub_menu::Top);
				};
				#children:
					(
						Text3dBundle {
							text_3d: Text3d {
								text: "Settings".into(),
								..default()
							},
							material: mats.add(new_unlit_material()),
							font: ui_fonts.mono_3d.clone(),
							transform: Transform {
								translation: Vec3::Z * 6.5,
								..default()
							},
							..default()
						},
					),
			),
	));
}

#[derive(Component, Debug, Reflect)]
#[reflect(Component)]
pub struct SettingsMenu;

#[derive(EnumComponent, Debug)]
pub enum SettingsSubMenu {
	Top,
	InputMap,
}
