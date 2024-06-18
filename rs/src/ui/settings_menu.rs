use crate::ui::pause_menu::pause_menu_widget;
use bevy::prelude::*;
use engine::{
	draw::{polygon_points, PlanarPolyLine},
	entity_tree,
	ui::{
		focus::{
			AdjacentWidgets, FocusTarget,
			FocusTarget::{NextSibling, PrevSibling, Sibling},
			Wedge2d,
		},
		layout::{RadialArrangement, RadialChildren},
		widgets::{
			dbg_event, new_unlit_material, on_back, on_ok, Button3dBundle, CuboidPanel,
			CuboidPanelBundle, CylinderFaces, CylinderPanel, CylinderPanelBundle, InteractHandlers,
			Node3dBundle, Text3d, Text3dBundle, WidgetBundle, WidgetShape,
		},
		Fade, FadeCommands, GlobalUi, MenuRef, MenuStack, UiAction, UiCam, UiFonts, UiMat,
		UiMatBuilder, GLOBAL_UI_RENDER_LAYERS,
	},
	util::{Angle, Flat},
};
use enum_components::{EntityEnumCommands, EnumComponent, WithVariant};
use rapier3d::prelude::SharedShape;
use std::{
	f32::consts::{FRAC_PI_2, FRAC_PI_3, FRAC_PI_6},
	ops::{ControlFlow, ControlFlow::Break},
};

pub mod ctrls_menu;

pub struct SettingsMenuPlugin;

impl Plugin for SettingsMenuPlugin {
	fn build(&self, app: &mut App) {
		app.init_resource::<SettingsSubMenus>()
			.add_systems(Startup, (setup, ctrls_menu::setup))
			.add_systems(PostUpdate, ctrls_menu::anchor_follow_focus);
	}
}

pub fn setup(
	mut cmds: Commands,
	mut meshes: ResMut<Assets<Mesh>>,
	mut mats: ResMut<Assets<UiMat>>,
	ui_fonts: Res<UiFonts>,
	mut sub_menus: ResMut<SettingsSubMenus>,
) {
	let adjacent = AdjacentWidgets {
		prev: Some(PrevSibling),
		next: Some(NextSibling),
		directions: (0..6)
			.map(|i| {
				(
					Wedge2d::sixth(Vec2::from_angle((FRAC_PI_3 * i as f32) + FRAC_PI_6)),
					Sibling(i),
				)
			})
			.collect(),
	};
	let text_material = mats.add(new_unlit_material());
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
					material: mats.add(UiMatBuilder::from(Color::rgb(0.5, 0.8, 0.7))),
					font: ui_fonts.mono_3d.clone(),
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
					handlers: MenuStack::pop_on_back(GLOBAL_UI_RENDER_LAYERS, 0.7),
					..default()
				},
				meshes.add(PlanarPolyLine {
					points: polygon_points(6, 10.5, 0.0),
					colors: vec![vec![Color::GRAY]],
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
				AdjacentWidgets {
					prev: Some(FocusTarget::Child(4)),
					next: Some(FocusTarget::Child(0)),
					directions: (0..5).map(|i| (
						Wedge2d::sixth(Vec2::from_angle((FRAC_PI_3 * i as f32) + (FRAC_PI_6 * 5.0))),
						FocusTarget::Child(i),
					)).collect(),
				},
				=> |cmds| {
					sub_menus.top = MenuRef::new(cmds.id());
					cmds.set_enum(settings_sub_menu::Top);
				};
				#children:
					(
						Button3dBundle {
							shape: WidgetShape { shape: SharedShape::cuboid(2.5, 0.5, 0.75), ..default() },
							mesh: meshes.add(Cuboid::new(5.0, 1.0, 1.5)),
							material: mats.add(UiMatBuilder::from(Color::CYAN.with_a(0.4))),
							..default()
						},
						adjacent.clone();
						#children:
							(
								Text3dBundle {
									text_3d: Text3d { text: "Gameplay".into(), ..default() },
									material: text_material.clone(),
									font: ui_fonts.mono_3d.clone(),
									transform: Transform::from_translation(Vec3::NEG_Y * 0.5),
									..default()
								},
							)
					),
					(
						Button3dBundle {
							shape: WidgetShape { shape: SharedShape::cuboid(2.5, 0.5, 0.75), ..default() },
							mesh: meshes.add(Cuboid::new(5.0, 1.0, 1.5)),
							material: mats.add(UiMatBuilder::from(Color::YELLOW.with_a(0.4))),
							..default()
						},
						adjacent.clone();
						#children:
							(
								Text3dBundle {
									text_3d: Text3d { text: "Graphics".into(), ..default() },
									material: text_material.clone(),
									font: ui_fonts.mono_3d.clone(),
									transform: Transform::from_translation(Vec3::NEG_Y * 0.5),
									..default()
								},
							)
					),
					(
						Button3dBundle {
							shape: WidgetShape { shape: SharedShape::cuboid(3.0, 0.5, 0.75), ..default() },
							mesh: meshes.add(Cuboid::new(6.0, 1.0, 1.5)),
							material: mats.add(UiMatBuilder::from(Color::FUCHSIA.with_a(0.4))),
							..default()
						},
						adjacent.clone();
						#children:
							(
								Text3dBundle {
									text_3d: Text3d { text: "accessibility".into(), ..default() },
									material: text_material.clone(),
									font: ui_fonts.mono_3d.clone(),
									transform: Transform::from_translation(Vec3::NEG_Y * 0.5),
									..default()
								},
							)
					),
					(
						Button3dBundle {
							shape: WidgetShape { shape: SharedShape::cuboid(2.5, 0.5, 0.75), ..default() },
							mesh: meshes.add(Cuboid::new(5.0, 1.0, 1.5)),
							material: mats.add(UiMatBuilder::from(Color::RED.with_a(0.4))),
							..default()
						},
						adjacent.clone();
						#children:
							(
								Text3dBundle {
									text_3d: Text3d { text: "Sound".into(), ..default() },
									material: text_material.clone(),
									font: ui_fonts.mono_3d.clone(),
									transform: Transform::from_translation(Vec3::NEG_Y * 0.5),
									..default()
								},
							)
					),
					(
						Button3dBundle {
							shape: WidgetShape { shape: SharedShape::cuboid(2.5, 0.5, 0.75), ..default() },
							mesh: meshes.add(Cuboid::new(5.0, 1.0, 1.5)),
							material: mats.add(UiMatBuilder::from(Color::GREEN.with_a(0.4))),
							handlers: InteractHandlers::on_ok(|cmds| {
								cmds.commands().add(|world: &mut World| {
									let menu = world.resource::<SettingsSubMenus>().controls;
									let mut q = world.query_filtered::<&mut MenuStack, With<GlobalUi>>();
									q.single_mut(world).push(menu);
									world.entity_mut(menu.root).fade_in_secs(1.5);
								});
								Break(())
							}),
							..default()
						},
						adjacent.clone();
						#children:
							(
								Text3dBundle {
									text_3d: Text3d { text: "Controls".into(), ..default() },
									material: text_material.clone(),
									font: ui_fonts.mono_3d.clone(),
									transform: Transform::from_translation(Vec3::NEG_Y * 0.5),
									..default()
								},
							)
					),
					(
						Button3dBundle {
							shape: WidgetShape { shape: SharedShape::cuboid(2.5, 0.5, 0.75), ..default() },
							mesh: meshes.add(Cuboid::new(5.0, 1.0, 1.5)),
							material: mats.add(UiMatBuilder::from(Color::BLUE.with_a(0.4))),
							..default()
						},
						adjacent.clone();
						#children:
							(
								Text3dBundle {
									text_3d: Text3d { text: "Kumquats".into(), ..default() },
									material: text_material.clone(),
									font: ui_fonts.mono_3d.clone(),
									transform: Transform::from_translation(Vec3::NEG_Y * 0.5),
									..default()
								},
							)
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
