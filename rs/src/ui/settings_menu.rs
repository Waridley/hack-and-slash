use crate::player::input::PlayerAction;
use bevy::color::palettes::basic::{AQUA, BLUE, FUCHSIA, GRAY, GREEN, RED, YELLOW};
use bevy::color::palettes::css::DARK_GRAY;
use bevy::prelude::*;
use engine::draw::rect_points;
use engine::input::map::icons::InputIconFileMap;
use engine::ui::widgets::borders::Border;
use engine::ui::widgets::{focus_toggle_border, Node3d};
use engine::{
	draw::{polygon_points, PlanarPolyLine},
	entity_tree,
	ui::{
		focus::{AdjacentWidgets, FocusTarget, Wedge2d},
		layout::{ExpandToFitChildren, RadialArrangement, RadialChildren},
		text::UiFonts,
		widgets::{
			dbg_event, new_unlit_material, on_ok, CuboidPanel, CuboidPanelBundle, CylinderPanel,
			CylinderPanelBundle, InteractHandlers, Text3d, Text3dBundle, WidgetShape,
		},
		Fade, FadeCommands, GlobalUi, MenuRef, MenuStack, UiAction, UiMat, UiMatBuilder,
		GLOBAL_UI_RENDER_LAYERS,
	},
	util::{Angle, Flat},
};
use enum_components::{EntityEnumCommands, EnumComponent};
use rapier3d::prelude::SharedShape;
use smallvec::smallvec;
use std::{
	f32::consts::{FRAC_PI_3, FRAC_PI_6},
	ops::ControlFlow::Break,
};

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
					(
						ctrls_menu::update_binding_list_widgets::<UiAction>,
						ctrls_menu::update_binding_list_widgets::<PlayerAction>,
					)
						.run_if(resource_exists::<InputIconFileMap>),
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
	let text_material = MeshMaterial3d(mats.add(new_unlit_material()));
	let blue_green_mat =
		MeshMaterial3d(mats.add(UiMatBuilder::from(Color::linear_rgb(0.5, 0.8, 0.7))));
	let button_focus_border_bundle = (
		Node3d,
		Transform {
			translation: Vec3::NEG_Y * 0.5,
			..default()
		},
		Visibility::Hidden,
		Border {
			margin: Vec2::splat(0.25),
			cross_section: rect_points(0.1, 0.1),
			..default()
		},
		blue_green_mat.clone(),
	);
	let focus_toggle_border = InteractHandlers(smallvec![dbg_event(), focus_toggle_border(),]);
	entity_tree!(cmds; (
		Name::new("SettingsMenu"),
		SettingsMenu,
		CylinderPanelBundle {
			panel: CylinderPanel {
				radius: 10.0,
				length: 6.0,
				..default()
			},
			material: MeshMaterial3d(mats.add(UiMatBuilder::from(StandardMaterial {
				base_color: LinearRgba::new(0.1, 0.1, 0.1, 0.8).into(),
				alpha_mode: AlphaMode::Blend,
				double_sided: true,
				cull_mode: None,
				..default()
			}))),
			..default()
		},
		Transform {
			translation: Vec3::new(0.0, -32.0, -24.0),
			..default()
		},
		MenuStack::pop_on_back(GLOBAL_UI_RENDER_LAYERS, 0.7),
		Fade::ZERO;
		#children: [
			(
				Text3dBundle {
					text_3d: Text3d {
						text: "Settings".into(),
						font: ui_fonts.mono.clone(),
						flat: false,
						..default()
					},
					material: blue_green_mat.clone(),
					..default()
				},
				Transform {
					translation: Vec3::new(0.0, -3.5, 0.0),
					..default()
				},
			),
			(
				WidgetShape { shape: SharedShape::cylinder(3.0, 10.0), ..default() },
				Transform {
					translation: Vec3::NEG_Y * 3.5,
					..default()
				},
				AdjacentWidgets {
					prev: Some(ChildN(5)),
					next: Some(ChildN(0)),
					directions: (0..5).map(|i| (
						Wedge2d::sixth(Vec2::from_angle((FRAC_PI_3 * i as f32) + FRAC_PI_6)),
						ChildN(i),
					)).collect(),
				},
				Mesh3d(meshes.add(PlanarPolyLine {
					points: polygon_points(6, 10.5, 0.0),
					colors: smallvec![smallvec![LinearRgba::from(GRAY)]],
					..default()
				}.flat())),
				MeshMaterial3d(mats.add(UiMatBuilder::from(Color::from(DARK_GRAY)))),
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
				#children: [
					(
						CuboidPanelBundle {
							panel: CuboidPanel {
								size: Vec3::new(5.0, 1.0, 1.5),
								..default()
							},
							material: MeshMaterial3d(mats.add(UiMatBuilder::from(Color::from(AQUA.with_alpha(0.4))))),
							..default()
						},
						adjacent.clone(),
						focus_toggle_border.clone(),
						ExpandToFitChildren {
							margin: Vec3::splat(0.25),
							offset: Vec3::Y * 0.5,
							..default()
						};
						#children: [
							(
								Text3dBundle {
									text_3d: Text3d {
										text: "Gameplay".into(),
										font: ui_fonts.mono.clone(),
										..default()
									},
									material: text_material.clone(),
									..default()
								},
								Transform::from_translation(Vec3::NEG_Y * 0.5),
							),
							(button_focus_border_bundle.clone()),
						]
					),
					(
						CuboidPanelBundle {
							panel: CuboidPanel {
								size: Vec3::new(5.0, 1.0, 1.5),
								..default()
							},
							material: MeshMaterial3d(mats.add(UiMatBuilder::from(Color::from(YELLOW.with_alpha(0.4))))),
							..default()
						},
						adjacent.clone(),
						focus_toggle_border.clone(),
						ExpandToFitChildren {
							margin: Vec3::splat(0.25),
							offset: Vec3::Y * 0.5,
							..default()
						};
						#children: [
							(
								Text3dBundle {
									text_3d: Text3d {
										text: "Graphics".into(),
										font: ui_fonts.mono.clone(),
										..default()
									},
									material: text_material.clone(),
									..default()
								},
								Transform::from_translation(Vec3::NEG_Y * 0.5),
							),
							(button_focus_border_bundle.clone()),
						]
					),
					(
						CuboidPanelBundle {
							panel: CuboidPanel {
								size: Vec3::new(5.0, 1.0, 1.5),
								..default()
							},
							material: MeshMaterial3d(mats.add(UiMatBuilder::from(Color::from(FUCHSIA.with_alpha(0.40))))),
							..default()
						},
						adjacent.clone(),
						focus_toggle_border.clone(),
						ExpandToFitChildren {
							margin: Vec3::splat(0.25),
							offset: Vec3::Y * 0.5,
							..default()
						};
						#children: [
							(
								Text3dBundle {
									text_3d: Text3d {
										text: "Accessibility".into(),
										font: ui_fonts.mono.clone(),
										..default()
									},
									material: text_material.clone(),
									..default()
								},
								Transform::from_translation(Vec3::NEG_Y * 0.5),
							),
							(button_focus_border_bundle.clone()),
						]
					),
					(
						CuboidPanelBundle {
							panel: CuboidPanel {
								size: Vec3::new(5.0, 1.0, 1.5),
								..default()
							},
							material: MeshMaterial3d(mats.add(UiMatBuilder::from(Color::from(RED.with_alpha(0.4))))),
							..default()
						},
						adjacent.clone(),
						focus_toggle_border.clone(),
						ExpandToFitChildren {
							margin: Vec3::splat(0.25),
							offset: Vec3::Y * 0.5,
							..default()
						};
						#children: [
							(
								Text3dBundle {
									text_3d: Text3d {
										text: "Sound".into(),
										font: ui_fonts.mono.clone(),
										..default()
									},
									material: text_material.clone(),
									..default()
								},
								Transform::from_translation(Vec3::NEG_Y * 0.5),
							),
							(button_focus_border_bundle.clone()),
						]
					),
					(
						CuboidPanelBundle {
							panel: CuboidPanel {
								size: Vec3::new(5.0, 1.0, 1.5),
								..default()
							},
							material: MeshMaterial3d(mats.add(UiMatBuilder::from(Color::from(GREEN.with_alpha(0.4))))),
							..default()
						},
						focus_toggle_border.clone()
							.and([on_ok(|cmds| {
								cmds.commands().queue(|world: &mut World| {
									let menu = world.resource::<SettingsSubMenus>().controls;
									let mut q = world.query_filtered::<&mut MenuStack, With<GlobalUi>>();
									q.single_mut(world).push(menu);
									world.entity_mut(menu.root).fade_in_secs(1.5);
								});
								Break(())
							})]),
						adjacent.clone(),
						ExpandToFitChildren {
							margin: Vec3::splat(0.25),
							offset: Vec3::Y * 0.5,
							..default()
						};
						#children: [
							(
								Text3dBundle {
									text_3d: Text3d {
										text: "Controls".into(),
										font: ui_fonts.mono.clone(),
										..default()
									},
									material: text_material.clone(),
									..default()
								},
								Transform::from_translation(Vec3::NEG_Y * 0.5),
							),
							(button_focus_border_bundle.clone()),
						]
					),
					(
						CuboidPanelBundle {
							panel: CuboidPanel {
								size: Vec3::new(5.0, 1.0, 1.5),
								..default()
							},
							material: MeshMaterial3d(mats.add(UiMatBuilder::from(Color::from(BLUE.with_alpha(0.4))))),
							..default()
						},
						adjacent.clone(),
						focus_toggle_border.clone(),
						ExpandToFitChildren {
							margin: Vec3::splat(0.25),
							offset: Vec3::Y * 0.5,
							..default()
						};
						#children: [
							(
								Text3dBundle {
									text_3d: Text3d {
										text: "Kumquats".into(),
										font: ui_fonts.mono.clone(),
										..default()
									},
									material: text_material.clone(),
									..default()
								},
								Transform::from_translation(Vec3::NEG_Y * 0.5),
							),
							(button_focus_border_bundle.clone()),
						]
					),
				]
			),
		]
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
