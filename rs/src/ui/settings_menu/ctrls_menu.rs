use crate::ui::settings_menu::SettingsSubMenus;
use bevy::{
	ecs::query::QueryEntityError,
	prelude::*,
	reflect::{Enum, EnumInfo, TypeInfo, Typed},
	render::view::RenderLayers,
};
use engine::{
	entity_tree,
	input::map::{
		icons::{Icon, InputIcons, UserInputIcons},
		widgets::{InputIcon, InputIconBundle},
		GamepadSeries,
	},
	todo_warn,
	ui::{
		focus::{AdjacentWidgets, FocusTarget},
		layout::{ExpandToFitChildren, LineUpChildren, RadialChildren},
		text::UiFonts,
		widgets::{
			dbg_event, focus_state_colors, new_unlit_material, on_focus, CuboidContainer,
			CuboidContainerBundle, CuboidPanel, CuboidPanelBundle, InteractHandlers, Text3d,
			Text3dBundle,
		},
		Fade, FadeCommands, GlobalUi, MenuRef, MenuStack, UiAction, UiMat, UiMatBuilder,
		GLOBAL_UI_RENDER_LAYERS,
	},
	util::LerpSlerp,
};
use leafwing_input_manager::{
	prelude::{InputKind, InputMap, UserInput},
	Actionlike,
};
use serde::{Deserialize, Serialize};
use std::{
	ops::{ControlFlow, ControlFlow::Break},
	sync::Arc,
};
use bevy::utils::CowArc;
use bevy::utils::smallvec::smallvec;
use engine::draw::{square_points, PlanarPolyLine};
use engine::input::ActionExt;
use engine::ui::widgets::borders::Border;
use engine::ui::widgets::{focus_toggle_border, InteractionKind, InteractionSource, Node3dBundle};
use engine::util::Flat;
use crate::player::BelongsToPlayer;
use crate::player::input::PlayerAction;

const GAME_BINDINGS_CONTAINER_NAME: &'static str = "GameBindingsContainer";
const UI_BINDINGS_CONTAINER_NAME: &'static str = "UiBindingsContainer";

pub fn setup(
	mut cmds: Commands,
	mut meshes: ResMut<Assets<Mesh>>,
	mut mats: ResMut<Assets<UiMat>>,
	ui_fonts: Res<UiFonts>,
	mut sub_menus: ResMut<SettingsSubMenus>,
) {
	let text_mat = mats.add(new_unlit_material());

	let entry_btn_mat = mats.add(UiMatBuilder::from(StandardMaterial {
		reflectance: 0.2,
		..StandardMaterial::from(Color::DARK_GRAY.with_a(0.5))
	}));
	
	fn bindings_entries<A: ActionExt>(
		cmds: &mut Commands,
		actions: impl Iterator<Item = A>,
		text_mat: Handle<UiMat>,
		entry_btn_mat: Handle<UiMat>,
		entry_focus_border_mat: Handle<UiMat>,
		font: Handle<Font>,
		owner: BelongsToPlayer,
	) -> Vec<Entity> {
		actions
			.map(move |action| {
				let name = action.display_name();
				
				let action_key = action.clone();
				entity_tree!(cmds; (
					CuboidContainerBundle {
						adjacent: AdjacentWidgets::all(FocusTarget::ChildN(1)),
						handlers: InteractHandlers::on_action(UiAction::Opt1, move |cmds| {
							let action_key = action_key.clone();
							cmds.commands().add(move |world: &mut World| {
								let mut q = world.query::<(&mut InputMap<A>, &BelongsToPlayer)>();
								let Some(mut imap) = q.iter_mut(world)
									.find(|(imap, &imap_owner)| imap_owner == owner)
									.map(|(imap, _)| imap)
								else {
									error!("failed to find imap for player {owner:?}");
									return
								};
								action_key.reset_to_default(&mut *imap);
							});
							Break(())
						}),
						..default()
					},
					LineUpChildren::horizontal().with_spacing(0.3).with_alignment(Vec3::NEG_X),
					ExpandToFitChildren::default();
					#children: [
						(
							Text3dBundle {
								text_3d: Text3d {
									text: name.into(),
									align_origin: Vec3::new(0.0, 0.5, 0.5),
									vertex_scale: Vec3::splat(0.5),
									..default()
								},
								font: font.clone(),
								material: text_mat.clone(),
								..default()
							},
						),
						(
							CuboidPanelBundle {
								panel: CuboidPanel {
									size: Vec3::new(4.0, 1.0, 1.5),
									mesh_margin: Vec3::new(0.0, 0.0, 0.5),
									..default()
								},
								material: entry_btn_mat.clone(),
								handlers: smallvec![
									dbg_event(),
									focus_toggle_border(),
								].into(),
								adjacent: AdjacentWidgets::vertical(
									"../-1/#1".parse().unwrap(),
									"../+1/#1".parse().unwrap(),
								),
								..default()
							},
							ExpandToFitChildren {
								margin: Vec3::new(0.25, 0.0, 0.25),
								min_size: Vec3::ONE,
								offset: Vec3::Y * 0.5,
								..default()
							},
							;
							#children: [
								(
									Border {
										cross_section: square_points(0.1),
										margin: Vec2::new(0.0, -1.0),
										..default()
									},
									Node3dBundle {
										visibility: Visibility::Hidden,
										transform: Transform {
											translation: Vec3::NEG_Y,
											..default()
										},
										..default()
									},
									entry_focus_border_mat.clone(),
								),
								(
									CuboidContainerBundle {
										transform: Transform {
											translation: Vec3::NEG_Y * 0.5,
											..default()
										},
										..default()
									},
									BindingListContainer(action),
									LineUpChildren::horizontal().with_spacing(0.25),
									ExpandToFitChildren::default(),
									owner,
								),
							]
						),
					]
				)).id()
			})
			.collect::<Vec<_>>()
	}
	
	let entry_focus_border_mat = mats.add(UiMatBuilder::from(Color::GRAY));
	
	//FIXME: Temporary till per-player Options menu is implemented
	let owner = BelongsToPlayer::new(1);
	
	let game_bindings_entries = bindings_entries(
		&mut cmds,
		PlayerAction::ALL.into_iter(),
		text_mat.clone(),
		entry_btn_mat.clone(),
		entry_focus_border_mat.clone(),
		ui_fonts.mono.clone(),
		owner,
	);
	
	let ui_bindings_entries = bindings_entries(
		&mut cmds,
		UiAction::ALL.into_iter(),
		text_mat.clone(),
		entry_btn_mat,
		entry_focus_border_mat,
		ui_fonts.mono.clone(),
		owner,
	);

	let transform = Transform {
		translation: Vec3::new(0.0, -48.0, -48.0),
		..default()
	};
	
	let all_first_control = AdjacentWidgets::all(format!("[{GAME_BINDINGS_CONTAINER_NAME}]/#0/#1").parse().unwrap());
	
	let bindings_section_components = (
		CuboidContainerBundle {
			adjacent: all_first_control.clone(),
			..default()
		},
		ExpandToFitChildren {
			margin: Vec3::new(1.0, 0.0, 1.0),
			..default()
		},
		LineUpChildren::vertical().with_alignment(Vec3::new(0.0, -20.0, -1.0)).with_spacing(1.0),
	);
	
	let bindings_section_border = (
		Border {
			margin: Vec2::splat(0.5),
			..default()
		},
		Node3dBundle::default(),
		mats.add(UiMatBuilder::from(Color::DARK_GRAY)),
	);
	
	let bindings_section_inner_components = (
		CuboidContainerBundle {
			adjacent: all_first_control.clone(),
			..default()
		},
		ExpandToFitChildren::default(),
		LineUpChildren::vertical().with_spacing(0.5),
	);
	
	let bindings_header_text = Text3dBundle {
		text_3d: Text3d {
			vertex_scale: Vec3::splat(0.7),
			text: "Menu Controls".into(),
			..default()
		},
		font: ui_fonts.mono.clone(),
		material: text_mat.clone(),
		..default()
	};
	
	let divider = (
		Name::new("divider"),
		CuboidPanelBundle {
			panel: CuboidPanel {
				size: Vec3::new(10.0, 0.25, 0.25),
				..default()
			},
			material: mats.add(UiMatBuilder::from(Color::DARK_GRAY)),
			..default()
		}
	);
	
	let bindings_container_components = (
		CuboidContainerBundle {
			adjacent: AdjacentWidgets::all("#0/#1".parse().unwrap()),
			..default()
		},
		LineUpChildren::vertical(),
		ExpandToFitChildren::default(),
	);
	
	let game_ctrls_section = entity_tree!(cmds; (
		Name::new("GameBindingsSection"),
		bindings_section_components.clone(),
		;
		#children: [
			(bindings_section_border.clone()),
			(
				bindings_section_inner_components.clone(),
				;
				#children: [
					(
						Text3dBundle {
							text_3d: Text3d {
								text: "Game Controls".into(),
								..bindings_header_text.text_3d.clone()
							},
							..bindings_header_text.clone()
						}
					),
					(divider.clone()),
					(
						Name::new(GAME_BINDINGS_CONTAINER_NAME),
						bindings_container_components.clone(),
						;
						=> |cmds| {
							cmds.push_children(&game_bindings_entries);
						}
					)
				]
			)
		]
	)).id();
	
	let ui_ctrls_section = entity_tree!(cmds; (
		Name::new("UiBindingsSection"),
		bindings_section_components,
		;
		#children: [
			(bindings_section_border),
			(
				bindings_section_inner_components,
				;
				#children: [
					(bindings_header_text),
					(divider),
					(
						Name::new(UI_BINDINGS_CONTAINER_NAME),
						bindings_container_components,
						;
						=> |cmds| {
							cmds.push_children(&ui_bindings_entries);
						}
					),
				]
			),
		]
	)).id();
	
	let root = entity_tree! { cmds;
		(
			Name::new("ControlsMenu"),
			CuboidPanelBundle {
				transform,
				material: mats.add(UiMatBuilder {
					std: StandardMaterial {
						base_color: Color::rgba(0.1, 0.3, 0.1, 0.5),
						alpha_mode: AlphaMode::Blend,
						cull_mode: None,
						double_sided: true,
						..default()
					},
					..default()
				}),
				handlers: MenuStack::pop_on_back(GLOBAL_UI_RENDER_LAYERS, 0.7),
				adjacent: all_first_control.clone(),
				..default()
			},
			ExpandToFitChildren {
				margin: Vec3::new(1.0, 0.0, 1.0),
				offset: Vec3::Y,
				..default()
			},
			Fade::ZERO,
			;
			#children: [
				(
					CuboidContainerBundle {
						transform: Transform {
							translation: Vec3::NEG_Y,
							..default()
						},
						adjacent: all_first_control.clone(),
						..default()
					},
					ExpandToFitChildren::default(),
					;
					#children: [
						(
							CuboidContainerBundle {
								adjacent: all_first_control.clone(),
								..default()
							},
							ExpandToFitChildren::default(),
							LineUpChildren::vertical().with_spacing(1.0),
							;
							#children: [
								(
									Text3dBundle {
										text_3d: Text3d {
											text: "Controls".into(),
											flat: false,
											..default()
										},
										font: ui_fonts.mono.clone(),
										material: mats.add(UiMatBuilder::from(Color::GREEN)),
										..default()
									}
								),
								(
									CuboidContainerBundle {
										adjacent: all_first_control.clone(),
										..default()
									},
									ExpandToFitChildren::default(),
									LineUpChildren::vertical().with_spacing(1.0),
									;
									=> |cmds| {
										cmds.push_children(&[
											game_ctrls_section,
											ui_ctrls_section,
										]);
									}
								)
							]
						),
					]
				),
			]
		)
	}
	.id();
	sub_menus.controls.root = root;
	sub_menus.controls.focus = root;
	sub_menus.controls.cam_target = cmds
		.spawn((TransformBundle::from_transform(transform), GLOBAL_UI_RENDER_LAYERS, CtrlsCamTarget))
		.id();
}

#[derive(Component, Debug, Reflect, Serialize, Deserialize)]
#[reflect(Component, Serialize, Deserialize, where for<'de> A: Serialize + Deserialize<'de>)]
pub struct BindingListContainer<A: Actionlike>(A);

#[derive(Component, Debug, Reflect, Serialize, Deserialize)]
#[reflect(Component, Serialize, Deserialize)]
pub struct CtrlsCamTarget;

pub fn anchor_follow_focus(
	mut q: Query<&mut Transform, With<CtrlsCamTarget>>,
	xforms: Query<&GlobalTransform>,
	sub_menus: Res<SettingsSubMenus>,
	mut stack: Query<&mut MenuStack, With<GlobalUi>>,
	t: Res<Time>,
) {
	let mut stack = stack.single_mut();
	let Some(mut menu) = stack.last_mut() else {
		return;
	};
	if menu.root != sub_menus.controls.root {
		return;
	}
	let focus = match xforms.get(menu.focus) {
		Ok(xform) => xform.compute_transform(),
		Err(e) => {
			error!("{e}");
			menu.focus = menu.root;
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
	if (target.translation.z - focus.translation.z).abs() > 6.0 {
		target.translation.z = target
			.translation
			.z
			.lerp(focus.translation.z, t.delta_seconds() * 10.0);
	}
	if target.rotation.angle_between(focus.rotation) > 0.001 {
		target.rotation = target
			.rotation
			.slerp(focus.rotation, t.delta_seconds() * 10.0);
	}
}

pub fn update_binding_list_widgets<A: Actionlike + std::fmt::Debug + Serialize>(
	mut cmds: Commands,
	q: Query<(Entity, &BindingListContainer<A>, Option<&BelongsToPlayer>)>,
	imaps: Query<(Ref<InputMap<A>>, &BelongsToPlayer)>,
	gamepads: Res<Gamepads>,
	mut global_imap: Option<ResMut<InputMap<A>>>,
	fonts: Res<UiFonts>,
	mut mats: ResMut<Assets<UiMat>>,
) {
	for (imap, owner) in global_imap
		.as_mut()
		.map(|mut it| (Ref::from(it.reborrow()), None))
		.into_iter()
		.chain(imaps.iter().map(|(imap, owner)| (imap, Some(*owner))))
		.filter(|(imap, _)| imap.is_changed())
	{
		debug!(action_type=std::any::type_name::<A>(), ?owner, imap=bevy::asset::ron::to_string(&*imap).unwrap());
		for (id, container, _) in q.iter().filter(|it| it.2.copied() == owner) {
			let gp = imap
				.gamepad()
				.and_then(|gp| gamepads.name(gp))
				.map(GamepadSeries::guess);
			let action = &container.0;
			let mut cmds = cmds.entity(id);
			debug!(?id,"clearing icons for {action:?}");
			cmds.despawn_descendants();
			cmds.with_children(|cmds| {
				let darker_gray = mats.add(UiMatBuilder::from(StandardMaterial {
					reflectance: 0.01,
					..StandardMaterial::from(Color::rgba(0.05, 0.05, 0.05, 0.7))
				}));
				let text_mat = mats.add(new_unlit_material());

				for binding in imap.get(action).into_iter().flatten() {
					fn icon_bundle(
						icon: Icon,
						fonts: &UiFonts,
						mat: Handle<UiMat>,
						scale: f32,
					) -> InputIconBundle<UiMat> {
						InputIconBundle::<UiMat> {
							input_icon: InputIcon {
								icon,
								size: Vec3::new(scale, 1.0, scale),
								tolerance: 0.2,
								..default()
							},
							font: fonts.mono.clone(),
							material: mat,
							..default()
						}
					}

					macro_rules! multi_icon {
						($cmds:ident, $icons:expr, $scale:expr) => {
							match $icons {
								InputIcons::Single(icon) => {
									let id = $cmds
										.spawn(icon_bundle(icon, &fonts, text_mat.clone(), $scale))
										.id();
								}
								InputIcons::DualAxis {
									vertical,
									horizontal,
								} => {
									$cmds.spawn(icon_bundle(
										vertical,
										&fonts,
										text_mat.clone(),
										$scale,
									));
									$cmds.spawn(Text3dBundle::<UiMat> {
										text_3d: Text3d {
											text: "/".into(),
											..default()
										},
										font: fonts.mono.clone(),
										material: text_mat.clone(),
										..default()
									});
									$cmds.spawn(icon_bundle(
										horizontal,
										&fonts,
										text_mat.clone(),
										$scale,
									));
								}
							}
						};
					}

					let icons = UserInputIcons::from_user_input(binding, gp);
					debug!(?action, ?binding, ?icons);

					cmds.spawn((
						CuboidPanelBundle {
							material: darker_gray.clone(),
							..default()
						},
						ExpandToFitChildren {
							offset: Vec3::Y * 0.51,
							..default()
						},
						LineUpChildren::horizontal().with_offset(Vec3::NEG_Y),
					))
					.with_children(|cmds| {
						const FULL_SIZE: f32 = 1.2;
						match icons {
							UserInputIcons::Single(icons) => multi_icon!(cmds, icons, FULL_SIZE),
							UserInputIcons::Chord(entries) => {
								let mut entries = entries.into_iter();
								if let Some(entry) = entries.next() {
									multi_icon!(cmds, entry, FULL_SIZE);
								}
								for entry in entries {
									cmds.spawn(Text3dBundle::<UiMat> {
										text_3d: Text3d {
											text: "+".into(),
											..default()
										},
										material: text_mat.clone(),
										font: fonts.mono.clone(),
										..default()
									});
									multi_icon!(cmds, entry, FULL_SIZE);
								}
							}
							UserInputIcons::VirtualDPad(dpad) => {
								let dpad = Arc::into_inner(dpad).expect(
									"icons were just created and are only in Arc for Reflect",
								);
								cmds.spawn((
									CuboidContainerBundle::default(),
									ExpandToFitChildren::default(),
									RadialChildren {
										radius: FULL_SIZE / 3.0,
										..default()
									},
								))
								.with_children(|cmds| {
									// Slightly oversized for readability.
									multi_icon!(cmds, dpad.right, FULL_SIZE / 2.4);
									multi_icon!(cmds, dpad.up, FULL_SIZE / 2.4);
									multi_icon!(cmds, dpad.left, FULL_SIZE / 2.4);
									multi_icon!(cmds, dpad.down, FULL_SIZE / 2.4);
								});
							}
							UserInputIcons::VirtualAxis(axis) => {
								let axis = Arc::into_inner(axis).expect(
									"icons were just created and are only in Arc for Reflect",
								);
								multi_icon!(cmds, axis.positive, FULL_SIZE);
								cmds.spawn(Text3dBundle::<UiMat> {
									text_3d: Text3d {
										text: "/".into(),
										..default()
									},
									material: text_mat.clone(),
									font: fonts.mono.clone(),
									..default()
								});
								multi_icon!(cmds, axis.negative, FULL_SIZE);
							}
						}
					});
				}
			});
		}
	}
}
