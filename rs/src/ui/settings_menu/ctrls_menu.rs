use crate::ui::settings_menu::SettingsSubMenus;
use bevy::{
	ecs::query::QueryEntityError,
	prelude::*,
	reflect::{Enum, EnumInfo, TypeInfo, Typed},
};
use engine::{
	entity_tree,
	ui::{
		focus::AdjacentWidgets,
		layout::{ExpandToFitChildren, LineUpChildren},
		widgets::{
			new_unlit_material, CuboidContainer, CuboidContainerBundle, CuboidPanel,
			CuboidPanelBundle, InteractHandlers, Text3d, Text3dBundle,
		},
		Fade, FadeCommands, GlobalUi, MenuRef, MenuStack, UiAction, UiFonts, UiMat, UiMatBuilder,
		GLOBAL_UI_RENDER_LAYERS,
	},
	util::LerpSlerp,
};
use serde::{Deserialize, Serialize};
use std::ops::ControlFlow::Break;

pub fn setup(
	mut cmds: Commands,
	mut meshes: ResMut<Assets<Mesh>>,
	mut mats: ResMut<Assets<UiMat>>,
	ui_fonts: Res<UiFonts>,
	mut sub_menus: ResMut<SettingsSubMenus>,
) {
	let text_mat = mats.add(new_unlit_material());

	let TypeInfo::Enum(info) = UiAction::type_info() else {
		unreachable!()
	};
	let mut entries = info
		.variant_names()
		.iter()
		.copied()
		.map(|name| {
			entity_tree!(cmds; (
				CuboidContainerBundle {
					..default()
				},
				LineUpChildren::horizontal().with_spacing(0.3).with_alignment(Vec3::NEG_X),
				ExpandToFitChildren::default(),
				AdjacentWidgets::vertical_siblings();
				#children:
					(
						Text3dBundle {
							text_3d: Text3d {
								text: name.into(),
								align_origin: Vec3::new(0.0, 0.5, 0.5),
								..default()
							},
							font: ui_fonts.mono_3d.clone(),
							material: text_mat.clone(),
							..default()
						},
					),
					(
						CuboidPanelBundle {
							panel: CuboidPanel { size: Vec3::new(4.0, 1.0, 1.0), ..default() },
							material: mats.add(UiMatBuilder::from(Color::ORANGE)),
							..default()
						},
					),
			))
			.id()
		})
		.collect::<Vec<_>>();
	let TEMP = cmds.entity(entries.remove(1)).despawn_recursive(); // FIXME: Fix text mesh generation.
	sub_menus.controls.focus = *entries.first().unwrap();

	let transform = Transform {
		translation: Vec3::new(0.0, -48.0, -48.0),
		..default()
	};
	let id = entity_tree! { cmds;
		(
			CuboidPanelBundle {
				transform,
				material: mats.add(UiMatBuilder {
					std: StandardMaterial {
						base_color: Color::rgba(0.1, 0.3, 0.1, 0.5),
						alpha_mode: AlphaMode::Blend,
						cull_mode: None,
						..default()
					},
					..default()
				}),
				handlers: MenuStack::pop_on_back(GLOBAL_UI_RENDER_LAYERS, 0.7),
				..default()
			},
			ExpandToFitChildren {
				margin: Vec3::new(1.0, 0.0, 1.0),
				offset: Vec3::Y * 1.0,
			},
			LineUpChildren::vertical().with_alignment(Vec3::NEG_Z).with_spacing(1.0),
			Fade::ZERO;
			#children:
				(
					Text3dBundle {
						text_3d: Text3d {
							text: "Controls".into(),
							flat: false,
							..default()
						},
						font: ui_fonts.mono_3d.clone(),
						material: mats.add(UiMatBuilder::from(Color::GREEN)),
						..default()
					}
				),
				(
					CuboidContainerBundle::default(),
					LineUpChildren::vertical().with_spacing(0.5),
					ExpandToFitChildren::default(),
					=> |cmds| {
						for id in entries {
							cmds.add_child(id);
						}
					}
				),
		)
	}
	.id();
	sub_menus.controls.root = id;
	sub_menus.controls.cam_target = cmds
		.spawn((TransformBundle::from_transform(transform), CtrlsCamTarget))
		.id();
}

#[derive(Component, Debug, Reflect, Serialize, Deserialize)]
#[reflect(Component, Serialize, Deserialize)]
pub struct CtrlsCamTarget;

pub fn anchor_follow_focus(
	mut q: Query<&mut Transform, With<CtrlsCamTarget>>,
	xforms: Query<&GlobalTransform>,
	sub_menus: Res<SettingsSubMenus>,
	stack: Query<&MenuStack, With<GlobalUi>>,
	t: Res<Time>,
) {
	let Some(menu) = stack.single().last() else {
		return;
	};
	if menu.root != sub_menus.controls.root {
		return;
	}
	let focus = match xforms.get(menu.focus) {
		Ok(xform) => xform.compute_transform(),
		Err(e) => {
			error!("{e}");
			return;
		}
	};
	let mut target = match q.get_mut(sub_menus.controls.cam_target) {
		Ok(target) => target,
		Err(e) => {
			error!("{e}");
			return;
		}
	};
	*target = target.lerp_slerp(focus, t.delta_seconds() * 10.0);
}
