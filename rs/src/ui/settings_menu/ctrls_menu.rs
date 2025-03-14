use crate::{
	player::{input::PlayerAction, BelongsToPlayer},
	ui::settings_menu::SettingsSubMenus,
};
use bevy::{
	color::palettes::basic::{GRAY, GREEN},
	prelude::*,
};
use engine::{
	draw::square_points,
	entity_tree,
	input::{
		map::{
			icons::{
				AxisIcons, ButtonIcons, DualAxisIcons, Icon, InputIconFileMap, TripleAxisIcons,
				UserInputIcons,
			},
			widgets::{InputIcon, InputIconBundle},
			Platform,
		},
		ActionExt,
	},
	ui::{
		focus::{AdjacentWidgets, FocusTarget},
		layout::{ExpandToFitChildren, LineUpChildren, RadialChildren},
		text::UiFonts,
		widgets::{
			borders::Border, dbg_event, focus_toggle_border, new_unlit_material, CuboidContainer,
			CuboidPanel, InteractHandlers, Node3d, Text3d,
		},
		Fade, GlobalUi, MenuStack, UiAction, UiMat, UiMatBuilder, GLOBAL_UI_RENDER_LAYERS,
	},
	util::MeshOutline,
};
use leafwing_input_manager::{prelude::InputMap, Actionlike};
use serde::{Deserialize, Serialize};
use smallvec::smallvec;
use std::{borrow::Cow, ops::ControlFlow::Break};

const GAME_BINDINGS_CONTAINER_NAME: &'static str = "GameBindingsContainer";
const UI_BINDINGS_CONTAINER_NAME: &'static str = "UiBindingsContainer";

pub fn setup(
	mut cmds: Commands,
	mut mats: ResMut<Assets<UiMat>>,
	ui_fonts: Res<UiFonts>,
	mut sub_menus: ResMut<SettingsSubMenus>,
) {
	let text_mat = mats.add(new_unlit_material());

	let entry_btn_mat = mats.add(UiMatBuilder::from(StandardMaterial {
		reflectance: 0.2,
		..StandardMaterial::from(Color::from(Srgba::gray(0.2).with_alpha(0.5)))
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
					CuboidContainer::default(),
					AdjacentWidgets::all(FocusTarget::ChildN(1)),
					InteractHandlers::on_action(UiAction::Opt1, move |cmds| {
						let action_key = action_key.clone();
						cmds.commands().queue(move |world: &mut World| {
							let mut q = world.query::<(&mut InputMap<A>, &BelongsToPlayer)>();
							let Some(mut imap) = q.iter_mut(world)
								.find(|(_, &imap_owner)| imap_owner == owner)
								.map(|(imap, _)| imap)
							else {
								error!("failed to find imap for player {owner:?}");
								return
							};
							action_key.reset_to_default(&mut *imap);
						});
						Break(())
					}),
					LineUpChildren::horizontal().with_spacing(0.3).with_alignment(Vec3::NEG_X),
					ExpandToFitChildren::default();
					#children: [
						(
							Text3d {
								text: name.into(),
								font: font.clone(),
								align_origin: Vec3::new(0.0, 0.5, 0.5),
								vertex_scale: Vec3::splat(0.5),
								..default()
							},
							MeshMaterial3d(text_mat.clone()),
						),
						(
							CuboidPanel {
								size: Vec3::new(4.0, 1.0, 1.5),
								mesh_margin: Vec3::new(0.0, 0.0, 0.5),
								..default()
							},
							MeshMaterial3d(entry_btn_mat.clone()),
							InteractHandlers(smallvec![
								dbg_event(),
								focus_toggle_border(),
							]),
							AdjacentWidgets::vertical(
								"../-1/#1".parse().unwrap(),
								"../+1/#1".parse().unwrap(),
							),
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
									Node3d,
									Visibility::Hidden,
									Transform {
										translation: Vec3::NEG_Y,
										..default()
									},
									MeshMaterial3d(entry_focus_border_mat.clone()),
								),
								(
									CuboidContainer::default(),
									Transform {
										translation: Vec3::NEG_Y * 0.5,
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
				))
				.id()
			})
			.collect::<Vec<_>>()
	}

	let entry_focus_border_mat = mats.add(UiMatBuilder::from(Color::from(GRAY)));

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

	let all_first_control = AdjacentWidgets::all(
		format!("[{GAME_BINDINGS_CONTAINER_NAME}]/#0/#1")
			.parse()
			.unwrap(),
	);

	let bindings_section_components = (
		CuboidContainer::default(),
		all_first_control.clone(),
		ExpandToFitChildren {
			margin: Vec3::new(1.0, 0.0, 1.0),
			..default()
		},
		LineUpChildren::vertical()
			.with_alignment(Vec3::new(0.0, -20.0, -1.0))
			.with_spacing(1.0),
	);

	let bindings_section_border = (
		Border {
			margin: Vec2::splat(0.5),
			..default()
		},
		Node3d,
		MeshMaterial3d(mats.add(UiMatBuilder::from(Color::from(Srgba::gray(0.2))))),
	);

	let bindings_section_inner_components = (
		CuboidContainer::default(),
		all_first_control.clone(),
		ExpandToFitChildren::default(),
		LineUpChildren::vertical().with_spacing(0.5),
	);

	let bindings_header_text = (
		Text3d {
			vertex_scale: Vec3::splat(0.7),
			text: "Menu Controls".into(),
			font: ui_fonts.mono.clone(),
			..default()
		},
		MeshMaterial3d(text_mat.clone()),
	);

	let divider = (
		Name::new("divider"),
		CuboidPanel {
			size: Vec3::new(10.0, 0.25, 0.25),
			..default()
		},
		MeshMaterial3d(mats.add(UiMatBuilder::from(Color::from(Srgba::gray(0.2))))),
	);

	let bindings_container_components = (
		CuboidContainer::default(),
		AdjacentWidgets::all("#0/#1".parse().unwrap()),
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
						Text3d {
							text: "Game Controls".into(),
							..bindings_header_text.0.clone()
						},
						bindings_header_text.1.clone()
					),
					(divider.clone()),
					(
						Name::new(GAME_BINDINGS_CONTAINER_NAME),
						bindings_container_components.clone(),
						;
						=> |cmds| {
							cmds.add_children(&game_bindings_entries);
						}
					)
				]
			)
		]
	))
	.id();

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
							cmds.add_children(&ui_bindings_entries);
						}
					),
				]
			),
		]
	))
	.id();

	let root = entity_tree! { cmds;
		(
			Name::new("ControlsMenu"),
			CuboidPanel::default(),
			MeshMaterial3d(mats.add(UiMatBuilder {
				std: StandardMaterial {
					base_color: Color::srgba(0.1, 0.3, 0.1, 0.5),
					alpha_mode: AlphaMode::Blend,
					cull_mode: None,
					double_sided: true,
					..default()
				},
				..default()
			})),
			transform,
			MenuStack::pop_on_back(GLOBAL_UI_RENDER_LAYERS, 0.7),
			all_first_control.clone(),
			ExpandToFitChildren {
				margin: Vec3::new(1.0, 0.0, 1.0),
				offset: Vec3::Y,
				..default()
			},
			Fade::ZERO,
			;
			#children: [
				(
					CuboidContainer::default(),
					Transform {
						translation: Vec3::NEG_Y,
						..default()
					},
					all_first_control.clone(),
					ExpandToFitChildren::default(),
					;
					#children: [
						(
							CuboidContainer::default(),
							all_first_control.clone(),
							ExpandToFitChildren::default(),
							LineUpChildren::vertical().with_spacing(1.0),
							;
							#children: [
								(
									Text3d {
										text: "Controls".into(),
										font: ui_fonts.mono.clone(),
										flat: false,
										..default()
									},
									MeshMaterial3d(mats.add(UiMatBuilder::from(Color::from(GREEN)))),
								),
								(
									CuboidContainer::default(),
									all_first_control.clone(),
									ExpandToFitChildren::default(),
									LineUpChildren::vertical().with_spacing(1.0),
									;
									=> |cmds| {
										cmds.add_children(&[
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
		.spawn((transform, GLOBAL_UI_RENDER_LAYERS, CtrlsCamTarget))
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
	let Some(menu) = stack.last_mut() else {
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
			.lerp(focus.translation.z, t.delta_secs() * 10.0);
	}
	if target.rotation.angle_between(focus.rotation) > 0.001 {
		target.rotation = target.rotation.slerp(focus.rotation, t.delta_secs() * 10.0);
	}
}

pub fn update_binding_list_widgets<A: Actionlike + std::fmt::Debug + Serialize>(
	mut cmds: Commands,
	q: Query<(Entity, &BindingListContainer<A>, Option<&BelongsToPlayer>)>,
	imaps: Query<(Ref<InputMap<A>>, &BelongsToPlayer)>,
	gamepads: Query<(Option<&Name>, &Gamepad)>,
	mut global_imap: Option<ResMut<InputMap<A>>>,
	fonts: Res<UiFonts>,
	mut mats: ResMut<Assets<UiMat>>,
	icon_map: Res<InputIconFileMap>,
) {
	for (imap, owner) in global_imap
		.as_mut()
		.map(|it| (Ref::from(it.reborrow()), None))
		.into_iter()
		.chain(imaps.iter().map(|(imap, owner)| (imap, Some(*owner))))
		.filter(|(imap, _)| imap.is_changed())
	{
		debug!(
			action_type = std::any::type_name::<A>(),
			?owner,
			imap = bevy::asset::ron::to_string(&*imap).unwrap()
		);
		let binding_entry_mat = MeshMaterial3d(mats.add(UiMatBuilder::from(StandardMaterial {
			reflectance: 0.01,
			..StandardMaterial::from(Color::srgba(0.12, 0.12, 0.12, 0.7))
		})));
		let text_mat = mats.add(UiMatBuilder::default());
		for (id, container, _) in q.iter().filter(|it| it.2.copied() == owner) {
			let gp = imap
				.gamepad()
				.and_then(|gp| gamepads.get(gp).ok())
				.and_then(|(name, _)| name)
				.map(Name::as_str)
				.and_then(Platform::guess_gamepad);
			let action = &container.0;
			let mut cmds = cmds.entity(id);
			debug!(?id, "clearing icons for {action:?}");
			cmds.despawn_descendants();
			cmds.with_children(|cmds| {
				let outline_mat = text_mat.clone();

				const OUTLINE: MeshOutline = MeshOutline::front_facing(0.1, 0.01);
				for binding in imap.get(action).into_iter().flatten() {
					fn icon_bundle(
						icon: Icon,
						font: Handle<Font>,
						mat: Handle<UiMat>,
						outline_mat: Handle<UiMat>,
						scale: f32,
					) -> InputIconBundle<UiMat> {
						InputIconBundle::<UiMat> {
							input_icon: InputIcon {
								icon,
								font,
								size: Vec3::new(scale, 0.5, scale),
								tolerance: 0.2,
								outline: Some(OUTLINE),
								outline_material: outline_mat.untyped(),
								..default()
							},
							material: MeshMaterial3d(mat),
							..default()
						}
					}

					fn separator(
						cmds: &mut ChildBuilder,
						text: impl Into<Cow<'static, str>>,
						scale: f32,
						font: Handle<Font>,
						text_mat: Handle<UiMat>,
						outline_mat: Handle<UiMat>,
					) {
						cmds.spawn((
							Text3d {
								text: text.into(),
								font,
								vertex_scale: Vec3::new(scale, 0.5, scale),
								..default()
							},
							MeshMaterial3d(text_mat.clone()),
						))
						.with_child((
							OUTLINE,
							MeshMaterial3d(outline_mat),
							GLOBAL_UI_RENDER_LAYERS,
						));
					}

					fn basic_icons(
						cmds: &mut ChildBuilder,
						icons: ButtonIcons,
						scale: f32,
						font: &Handle<Font>,
						text_mat: &Handle<UiMat>,
						outline_mat: &Handle<UiMat>,
					) {
						match icons {
							ButtonIcons::Simple(icon) => {
								cmds.spawn(icon_bundle(
									icon,
									font.clone(),
									text_mat.clone(),
									outline_mat.clone(),
									scale,
								));
							}
							ButtonIcons::Composite(btns) => {
								let mut btns = btns.into_iter();
								let btn = btns.next();

								// TODO: Use panel/border instead of brackets
								separator(
									cmds,
									"[",
									scale * 1.1,
									font.clone(),
									text_mat.clone(),
									outline_mat.clone(),
								);

								// Manual intersperse to avoid mutably borrowing cmds twice
								if let Some(btn) = btn {
									basic_icons(cmds, btn, scale, font, text_mat, outline_mat);

									for btn in btns {
										separator(
											cmds,
											"|",
											scale,
											font.clone(),
											text_mat.clone(),
											outline_mat.clone(),
										);
										basic_icons(cmds, btn, scale, font, text_mat, outline_mat);
									}
								}

								separator(
									cmds,
									"]",
									scale * 1.1,
									font.clone(),
									text_mat.clone(),
									outline_mat.clone(),
								);
							}
							ButtonIcons::Chord(btns) => {
								let mut btns = btns.into_iter();
								let btn = btns.next();

								// Manual intersperse to avoid mutably borrwing cmds twice
								if let Some(btn) = btn {
									basic_icons(cmds, btn, scale, font, text_mat, outline_mat);

									for btn in btns {
										separator(
											cmds,
											"+",
											scale,
											font.clone(),
											text_mat.clone(),
											outline_mat.clone(),
										);
										basic_icons(cmds, btn, scale, font, text_mat, outline_mat);
									}
								}
							}
						}
					}

					fn axis_icons(
						cmds: &mut ChildBuilder,
						icons: AxisIcons,
						scale: f32,
						font: &Handle<Font>,
						text_mat: &Handle<UiMat>,
						outline_mat: &Handle<UiMat>,
					) {
						match icons {
							AxisIcons::Single(icon) => {
								cmds.spawn(icon_bundle(
									icon,
									font.clone(),
									text_mat.clone(),
									outline_mat.clone(),
									scale,
								));
							}
							AxisIcons::Composite { positive, negative } => {
								// TODO: Use panel/border instead of brackets
								separator(
									cmds,
									"[",
									scale * 1.1,
									font.clone(),
									text_mat.clone(),
									outline_mat.clone(),
								);
								basic_icons(cmds, positive, scale, font, text_mat, outline_mat);
								separator(
									cmds,
									"|",
									scale,
									font.clone(),
									text_mat.clone(),
									outline_mat.clone(),
								);
								basic_icons(cmds, negative, scale, font, text_mat, outline_mat);
								separator(
									cmds,
									"]",
									scale * 1.1,
									font.clone(),
									text_mat.clone(),
									outline_mat.clone(),
								);
							}
							AxisIcons::Chord { button, axis } => {
								basic_icons(cmds, button, scale, font, text_mat, outline_mat);
								separator(
									cmds,
									"+",
									scale,
									font.clone(),
									text_mat.clone(),
									outline_mat.clone(),
								);
								axis_icons(
									cmds,
									(*axis).clone(),
									scale,
									font,
									text_mat,
									outline_mat,
								)
							}
						}
					}

					fn dual_axis_icons(
						cmds: &mut ChildBuilder,
						icons: DualAxisIcons,
						scale: f32,
						font: &Handle<Font>,
						text_mat: &Handle<UiMat>,
						outline_mat: &Handle<UiMat>,
					) {
						match icons {
							DualAxisIcons::Single(icon) => {
								cmds.spawn(icon_bundle(
									icon,
									font.clone(),
									text_mat.clone(),
									outline_mat.clone(),
									scale,
								));
							}
							DualAxisIcons::Composite {
								horizontal:
									AxisIcons::Composite {
										positive: right,
										negative: left,
									},
								vertical:
									AxisIcons::Composite {
										positive: up,
										negative: down,
									},
							} => {
								cmds.spawn((
									CuboidContainer::default(),
									ExpandToFitChildren::default(),
									RadialChildren {
										radius: scale / 3.0,
										..default()
									},
								))
								.with_children(|cmds| {
									// Slightly oversized for readability.
									basic_icons(
										cmds,
										right,
										scale / 2.4,
										font,
										text_mat,
										outline_mat,
									);
									basic_icons(cmds, up, scale / 2.4, font, text_mat, outline_mat);
									basic_icons(
										cmds,
										left,
										scale / 2.4,
										font,
										text_mat,
										outline_mat,
									);
									basic_icons(
										cmds,
										down,
										scale / 2.4,
										font,
										text_mat,
										outline_mat,
									);
								});
							}
							DualAxisIcons::Composite {
								horizontal,
								vertical,
							} => {
								// TODO: Layout vertical icons vertically?
								axis_icons(cmds, horizontal, scale, font, text_mat, outline_mat);
								separator(
									cmds,
									"|",
									scale,
									font.clone(),
									text_mat.clone(),
									outline_mat.clone(),
								);
								axis_icons(cmds, vertical, scale, font, text_mat, outline_mat);
							}
							DualAxisIcons::Chord { button, dual_axis } => {
								basic_icons(cmds, button, scale, font, text_mat, outline_mat);
								separator(
									cmds,
									"+",
									scale,
									font.clone(),
									text_mat.clone(),
									outline_mat.clone(),
								);
								dual_axis_icons(
									cmds,
									(*dual_axis).clone(),
									scale,
									font,
									text_mat,
									outline_mat,
								);
							}
						}
					}

					let icons = UserInputIcons::from_user_input(binding.clone(), gp, &*icon_map);
					debug!(?action, ?binding, ?icons);

					cmds.spawn((
						CuboidPanel::default(),
						binding_entry_mat.clone(),
						ExpandToFitChildren {
							offset: Vec3::Y * 0.51,
							..default()
						},
						LineUpChildren::horizontal().with_offset(Vec3::NEG_Y),
					))
					.with_children(|cmds| {
						const FULL_SIZE: f32 = 1.2;
						match icons {
							UserInputIcons::Button(icons) => basic_icons(
								cmds,
								icons,
								FULL_SIZE,
								&fonts.mono,
								&text_mat,
								&outline_mat,
							),
							UserInputIcons::Axis(icons) => axis_icons(
								cmds,
								icons,
								FULL_SIZE,
								&fonts.mono,
								&text_mat,
								&outline_mat,
							),
							UserInputIcons::DualAxis(icons) => dual_axis_icons(
								cmds,
								icons,
								FULL_SIZE,
								&fonts.mono,
								&text_mat,
								&outline_mat,
							),
							UserInputIcons::TripleAxis(icons) => match icons {
								TripleAxisIcons::Single(icon) => {
									cmds.spawn(icon_bundle(
										icon,
										fonts.mono.clone(),
										text_mat.clone(),
										outline_mat.clone(),
										FULL_SIZE,
									));
								}
								TripleAxisIcons::Composite { x, y, z } => {
									axis_icons(
										cmds,
										x,
										FULL_SIZE,
										&fonts.mono,
										&text_mat,
										&outline_mat,
									);
									separator(
										cmds,
										"|",
										FULL_SIZE,
										fonts.mono.clone(),
										text_mat.clone(),
										outline_mat.clone(),
									);
									axis_icons(
										cmds,
										y,
										FULL_SIZE,
										&fonts.mono,
										&text_mat,
										&outline_mat,
									);
									separator(
										cmds,
										"|",
										FULL_SIZE,
										fonts.mono.clone(),
										text_mat.clone(),
										outline_mat.clone(),
									);
									axis_icons(
										cmds,
										z,
										FULL_SIZE,
										&fonts.mono,
										&text_mat,
										&outline_mat,
									);
								}
							},
						}
					});
				}
			});
		}
	}
}
