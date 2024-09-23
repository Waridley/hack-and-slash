use crate::{player::input::PlayerAction, ui::pause_menu::pause_menu_widget};
use bevy::prelude::*;
use engine::{
	draw::{polygon_points, PlanarPolyLine},
	entity_tree,
	ui::{
		focus::{AdjacentWidgets, FocusTarget, FocusTarget::Sibling, Wedge2d},
		layout::{ExpandToFitChildren, RadialArrangement, RadialChildren},
		text::UiFonts,
		widgets::{
			dbg_event, focus_state_colors, new_unlit_material, on_back, on_ok, CuboidPanel,
			CuboidPanelBundle, CylinderFaces, CylinderPanel, CylinderPanelBundle, InteractHandlers,
			Node3dBundle, Text3d, Text3dBundle, WidgetBundle, WidgetShape,
		},
		Fade, FadeCommands, GlobalUi, MenuRef, MenuStack, UiAction, UiCam, UiMat, UiMatBuilder,
		GLOBAL_UI_RENDER_LAYERS,
	},
	util::{Angle, Flat},
};
use enum_components::{EntityEnumCommands, EnumComponent, WithVariant};
use rapier3d::prelude::SharedShape;
use std::{
	f32::consts::{FRAC_PI_2, FRAC_PI_3, FRAC_PI_6},
	ops::{ControlFlow, ControlFlow::Break},
};
use bevy::utils::smallvec::smallvec;
use engine::draw::rect_points;
use engine::ui::widgets::borders::Border;
use engine::ui::widgets::focus_toggle_border;

pub mod ctrls_menu;

pub struct SettingsMenuPlugin;

impl Plugin for SettingsMenuPlugin {
	fn build(&self, app: &mut App) {
		app.init_resource::<SettingsSubMenus>()
			.add_systems(Startup, (setup, ctrls_menu::setup))
			.add_systems(
				PostUpdate,
				(
					ctrls_menu::anchor_follow_focus,
					ctrls_menu::update_binding_list_widgets::<UiAction>,
					ctrls_menu::update_binding_list_widgets::<PlayerAction>,
				),
			);
	}
}

pub fn setup(
	mut cmds: Commands,
	mut meshes: ResMut<Assets<Mesh>>,
	mut mats: ResMut<Assets<UiMat>>,
	ui_fonts: Res<UiFonts>,
	mut sub_menus: ResMut<SettingsSubMenus>,
) {
	use FocusTarget::*;
	let adjacent = AdjacentWidgets {
		// Counter-clockwise order is intuitive in math but not for players
		prev: Some(Sibling(1)),
		next: Some(Sibling(-1)),
		directions: (0..6)
			.map(|i| {
				(
					Wedge2d::sixth(Vec2::from_angle((FRAC_PI_3 * i as f32) + FRAC_PI_6)),
					Path(vec![ToParent, ChildN(i)]),
				)
			})
			.collect(),
	};
	let text_material = mats.add(new_unlit_material());
	let blue_green_mat = mats.add(UiMatBuilder::from(Color::rgb(0.5, 0.8, 0.7)));
	let button_focus_border_bundle = (
		Node3dBundle {
			transform: Transform {
				translation: Vec3::NEG_Y * 0.5,
				..default()
			},
			visibility: Visibility::Hidden,
			..default()
		},
		Border {
			margin: Vec2::splat(0.25),
			cross_section: rect_points(0.1, 0.1),
			..default()
		},
		blue_green_mat.clone()
	);
	let focus_toggle_border = InteractHandlers(smallvec![
		dbg_event(),
		focus_toggle_border(),
	]);
	entity_tree!(cmds; (
		Name::new("SettingsMenu"),
		SettingsMenu,
		CylinderPanelBundle {
			panel: CylinderPanel {
				radius: 10.0,
				length: 6.0,
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
			handlers: MenuStack::pop_on_back(GLOBAL_UI_RENDER_LAYERS, 0.7),
			..default()
		},
		Fade::ZERO;
		#children:
			(
				Text3dBundle {
					text_3d: Text3d {
						text: "Settings".into(),
						flat: false,
						..default()
					},
					material: blue_green_mat.clone(),
					font: ui_fonts.mono.clone(),
					transform: Transform {
						translation: Vec3::new(0.0, -3.5, 0.0),
						..default()
					},
					..default()
				},
			),
			(
				WidgetBundle {
					shape: WidgetShape { shape: SharedShape::cylinder(3.0, 10.0), ..default() },
					transform: Transform {
						translation: Vec3::NEG_Y * 3.5,
						..default()
					},
					adjacent: AdjacentWidgets {
						prev: Some(ChildN(5)),
						next: Some(ChildN(0)),
						directions: (0..5).map(|i| (
							Wedge2d::sixth(Vec2::from_angle((FRAC_PI_3 * i as f32) + FRAC_PI_6)),
							ChildN(i),
						)).collect(),
					},
					..default()
				},
				meshes.add(PlanarPolyLine {
					points: polygon_points(6, 10.5, 0.0),
					colors: smallvec![smallvec![Color::GRAY]],
					..default()
				}.flat()),
				mats.add(UiMatBuilder::from(Color::DARK_GRAY)),
				RadialChildren {
					radius: 7.0,
					arrangement: RadialArrangement::Manual {
						separation: Angle::Rad(FRAC_PI_3),
						first: Angle::Rad(FRAC_PI_6),
					},
					..default()
				},
				;
				=> |cmds| {
					sub_menus.top = MenuRef::new(cmds.id());
					cmds.set_enum(settings_sub_menu::Top);
				}
				#children:
					(
						CuboidPanelBundle {
							panel: CuboidPanel {
								size: Vec3::new(5.0, 1.0, 1.5),
								..default()
							},
							material: mats.add(UiMatBuilder::from(Color::CYAN.with_a(0.4))),
							adjacent: adjacent.clone(),
							handlers: focus_toggle_border.clone(),
							..default()
						},
						ExpandToFitChildren {
							margin: Vec3::splat(0.25),
							offset: Vec3::Y * 0.5,
							..default()
						};
						#children:
							(
								Text3dBundle {
									text_3d: Text3d { text: "Gameplay".into(), ..default() },
									material: text_material.clone(),
									font: ui_fonts.mono.clone(),
									transform: Transform::from_translation(Vec3::NEG_Y * 0.5),
									..default()
								},
							),
							(button_focus_border_bundle.clone()),
					),
					(
						CuboidPanelBundle {
							panel: CuboidPanel {
								size: Vec3::new(5.0, 1.0, 1.5),
								..default()
							},
							material: mats.add(UiMatBuilder::from(Color::YELLOW.with_a(0.4))),
							adjacent: adjacent.clone(),
							handlers: focus_toggle_border.clone(),
							..default()
						},
						ExpandToFitChildren {
							margin: Vec3::splat(0.25),
							offset: Vec3::Y * 0.5,
							..default()
						};
						#children:
							(
								Text3dBundle {
									text_3d: Text3d { text: "Graphics".into(), ..default() },
									material: text_material.clone(),
									font: ui_fonts.mono.clone(),
									transform: Transform::from_translation(Vec3::NEG_Y * 0.5),
									..default()
								},
							),
							(button_focus_border_bundle.clone()),
					),
					(
						CuboidPanelBundle {
							panel: CuboidPanel {
								size: Vec3::new(5.0, 1.0, 1.5),
								..default()
							},
							material: mats.add(UiMatBuilder::from(Color::FUCHSIA.with_a(0.4))),
							adjacent: adjacent.clone(),
							handlers: focus_toggle_border.clone(),
							..default()
						},
						ExpandToFitChildren {
							margin: Vec3::splat(0.25),
							offset: Vec3::Y * 0.5,
							..default()
						};
						#children:
							(
								Text3dBundle {
									text_3d: Text3d { text: "Accessibility".into(), ..default() },
									material: text_material.clone(),
									font: ui_fonts.mono.clone(),
									transform: Transform::from_translation(Vec3::NEG_Y * 0.5),
									..default()
								},
							),
							(button_focus_border_bundle.clone()),
					),
					(
						CuboidPanelBundle {
							panel: CuboidPanel {
								size: Vec3::new(5.0, 1.0, 1.5),
								..default()
							},
							material: mats.add(UiMatBuilder::from(Color::RED.with_a(0.4))),
							adjacent: adjacent.clone(),
							handlers: focus_toggle_border.clone(),
							..default()
						},
						ExpandToFitChildren {
							margin: Vec3::splat(0.25),
							offset: Vec3::Y * 0.5,
							..default()
						};
						#children:
							(
								Text3dBundle {
									text_3d: Text3d { text: "Sound".into(), ..default() },
									material: text_material.clone(),
									font: ui_fonts.mono.clone(),
									transform: Transform::from_translation(Vec3::NEG_Y * 0.5),
									..default()
								},
							),
							(button_focus_border_bundle.clone()),
					),
					(
						CuboidPanelBundle {
							panel: CuboidPanel {
								size: Vec3::new(5.0, 1.0, 1.5),
								..default()
							},
							material: mats.add(UiMatBuilder::from(Color::GREEN.with_a(0.4))),
							handlers: focus_toggle_border.clone()
							.and([on_ok(|cmds| {
									cmds.commands().add(|world: &mut World| {
										let menu = world.resource::<SettingsSubMenus>().controls;
										let mut q = world.query_filtered::<&mut MenuStack, With<GlobalUi>>();
										q.single_mut(world).push(menu);
										world.entity_mut(menu.root).fade_in_secs(1.5);
									});
									Break(())
								})]),
							adjacent: adjacent.clone(),
							..default()
						},
						ExpandToFitChildren {
							margin: Vec3::splat(0.25),
							offset: Vec3::Y * 0.5,
							..default()
						};
						#children:
							(
								Text3dBundle {
									text_3d: Text3d { text: "Controls".into(), ..default() },
									material: text_material.clone(),
									font: ui_fonts.mono.clone(),
									transform: Transform::from_translation(Vec3::NEG_Y * 0.5),
									..default()
								},
							),
							(button_focus_border_bundle.clone()),
					),
					(
						CuboidPanelBundle {
							panel: CuboidPanel {
								size: Vec3::new(5.0, 1.0, 1.5),
								..default()
							},
							material: mats.add(UiMatBuilder::from(Color::BLUE.with_a(0.4))),
							adjacent: adjacent.clone(),
							handlers: focus_toggle_border.clone(),
							..default()
						},
						ExpandToFitChildren {
							margin: Vec3::splat(0.25),
							offset: Vec3::Y * 0.5,
							..default()
						};
						#children:
							(
								Text3dBundle {
									text_3d: Text3d { text: "Kumquats".into(), ..default() },
									material: text_material.clone(),
									font: ui_fonts.mono.clone(),
									transform: Transform::from_translation(Vec3::NEG_Y * 0.5),
									..default()
								},
							),
							(button_focus_border_bundle.clone()),
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
	Accessibility,
	Gameplay,
	Controls,
	Graphics,
	Sound,
}

#[derive(Resource, Debug, Reflect)]
#[reflect(Resource)]
pub struct SettingsSubMenus {
	pub top: MenuRef,
	pub accessibility: MenuRef,
	pub gameplay: MenuRef,
	pub controls: MenuRef,
	pub graphics: MenuRef,
	pub sound: MenuRef,
}

impl Default for SettingsSubMenus {
	fn default() -> Self {
		Self {
			top: MenuRef::INVALID,
			accessibility: MenuRef::INVALID,
			gameplay: MenuRef::INVALID,
			controls: MenuRef::INVALID,
			graphics: MenuRef::INVALID,
			sound: MenuRef::INVALID,
		}
	}
}
