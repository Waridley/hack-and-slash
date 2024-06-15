use crate::{
	player::input::PlayerAction,
	ui::{
		settings_menu,
		settings_menu::{settings_sub_menu, SettingsMenu},
	},
};
use bevy::{
	app::AppExit,
	ecs::{
		query::QuerySingleError,
		system::{RunSystemOnce, SystemId},
	},
	pbr::ExtendedMaterial,
	prelude::*,
};
use engine::{
	anim::StartAnimation,
	draw::PlanarPolyLine,
	entity_tree,
	input::InputState,
	mats::{
		fade::DitherFade,
		fog::{DistanceDither, Matter},
		ExtMat,
	},
	todo_warn,
	ui::{
		focus::{AdjacentWidgets, FocusTarget, Wedge2d},
		layout::LineUpChildren,
		widgets::{
			dbg_event, new_unlit_material, on_ok, Button3dBundle, CuboidFaces, CuboidPanel,
			CuboidPanelBundle, InteractHandlers, Node3dBundle, RectCorners, Text3d, Text3dBundle,
			WidgetBundle, WidgetShape,
		},
		Fade, FadeCommands, GlobalUi, MenuStack, UiCam, UiFonts, UiMat, UiMatBuilder,
	},
	util::{Flat, StateStack},
};
use enum_components::{EntityEnumCommands, EnumComponent, WithVariant};
use leafwing_input_manager::action_state::ActionState;
use rapier3d::geometry::SharedShape;
use std::ops::ControlFlow;
use web_time::Duration;

pub struct PauseMenuPlugin;

impl Plugin for PauseMenuPlugin {
	fn build(&self, app: &mut App) {
		app.init_resource::<PauseSystems>()
			.add_systems(Startup, setup)
			.add_systems(Update, show_pause_menu);
	}
}

#[derive(Resource, Debug)]
pub struct PauseSystems {
	pub pause: SystemId,
	pub unpause: SystemId,
}

impl FromWorld for PauseSystems {
	fn from_world(world: &mut World) -> Self {
		PauseSystems {
			pause: world.register_system(pause),
			unpause: world.register_system(unpause),
		}
	}
}

pub fn setup(
	mut cmds: Commands,
	mut mats: ResMut<Assets<UiMat>>,
	mut meshes: ResMut<Assets<Mesh>>,
	ui_fonts: Res<UiFonts>,
) {
	let btn_txt_mat = mats.add(UiMatBuilder {
		std: StandardMaterial {
			base_color: Color::BLACK,
			unlit: true,
			..default()
		},
		..default()
	});
	entity_tree!(cmds; (
		Name::new("PauseMenu"),
		CuboidPanelBundle {
			panel: CuboidPanel {
				size: Vec3::new(12.0, 12.0, 12.0),
				..default()
			},
			material: mats.add(UiMatBuilder {
				std: Color::rgba(0.5, 0.5, 0.5, 0.5).into(),
				..default()
			}),
			..default()
		},
		Fade::ZERO,
		AdjacentWidgets::all(FocusTarget::Child(0));
		#children:
			(
				Name::new("border"),
				Node3dBundle {
					transform: Transform {
						translation: Vec3::NEG_Y * 6.5,
						..default()
					},
					..default()
				},
				meshes.add(
					PlanarPolyLine {
						colors: vec![
							vec![Color::rgba(0.04, 0.005, 0.05, 0.8)],

						],
						..PlanarPolyLine::rect(12.0, 12.0, 0.5)
					}
					.flat()
				),
				mats.add(UiMatBuilder::default())
			),
			(
				Name::new("game_paused_text"),
				Text3dBundle {
					text_3d: Text3d {
						text: "Game Paused".into(),
						flat: false,
						..default()
					},
					material: mats.add(UiMatBuilder::from(Color::WHITE)),
					font: ui_fonts.mono_3d.clone(),
					transform: Transform {
						translation: Vec3::new(0.0, -6.5, 5.0),
						..default()
					},
					..default()
				},
			),
			(
				Name::new("container"),
				WidgetBundle {
					shape: WidgetShape(SharedShape::cuboid(5.5, 1.0, 5.5)),
					transform: Transform {
						translation: Vec3::NEG_Y * 6.5,
						..default()
					},
					..default()
				},
				LineUpChildren::vertical().with_spacing(0.1),
				AdjacentWidgets::all(FocusTarget::Child(0));
				#children:
					(
						Name::new("resume_btn"),
						Button3dBundle {
							shape: WidgetShape(SharedShape::cuboid(3.0, 0.6, 0.6)),
							mesh: meshes.add(Cuboid::new(6.0, 1.2, 1.2)),
							material: mats.add(UiMatBuilder {
								std: StandardMaterial {
									base_color: Color::BLACK,
									emissive: Color::LIME_GREEN * 16.0,
									..default()
								},
								..default()
							}),
							handlers: vec![
								dbg_event(),
								on_ok(|cmds| {
									cmds.commands().add(|world: &mut World| {
										world.run_system_once(unpause);
									});
									ControlFlow::Break(())
								}),
							].into(),
							..default()
						},
						AdjacentWidgets::vertical_siblings()
						=> |cmds| {
							cmds.set_enum(pause_menu_widget::ResumeButton);
						};
						#children: (
							Name::new("resume_btn_text"),
							Text3dBundle {
								text_3d: Text3d { text: "Resume Game".into(), ..default() },
								font: ui_fonts.mono_3d.clone(),
								material: btn_txt_mat.clone(),
								transform: Transform {
									translation: Vec3::NEG_Y * 0.61,
									..default()
								},
								..default()
							}
						),
					),
					(
						Name::new("settings_btn"),
						Button3dBundle {
							shape: WidgetShape(SharedShape::cuboid(3.0, 0.6, 0.6)),
							mesh: meshes.add(Cuboid::new(6.0, 1.2, 1.2)),
							material: mats.add(UiMatBuilder {
								std: StandardMaterial {
									base_color: Color::BLACK,
									emissive: Color::GRAY * 16.0,
									..default()
								},
								..default()
							}),
							handlers: vec![
								dbg_event(),
								on_ok(|cmds| {
									cmds.commands().add(|world: &mut World| {
										let mut q = world.query_filtered::<Entity, With<SettingsMenu>>();
										let panel_id = q.single(world);
										world.entity_mut(panel_id).fade_in_secs(2.0);

										let mut q = world.query_filtered::<Entity, WithVariant<settings_sub_menu::Top>>();
										let id = q.single(world);
										let mut q = world.query_filtered::<&mut MenuStack, With<GlobalUi>>();
										let mut stack = q.single_mut(world);
										stack.push(id);

										let mut q = world.query_filtered::<&mut UiCam, With<GlobalUi>>();
										let mut cam = q.single_mut(world);
										cam.focus = Some(id);
									});
									ControlFlow::Break(())
								}),
							].into(),
							..default()
						},
						AdjacentWidgets::vertical_siblings(),
						=> |cmds| {
							cmds.set_enum(pause_menu_widget::SettingsButton);
						};
						#children:
							(
								Name::new("settings_btn_text"),
								Text3dBundle {
									text_3d: Text3d { text: "Settings...".into(), ..default() },
									font: ui_fonts.mono_3d.clone(),
									material: btn_txt_mat.clone(),
									transform: Transform {
										translation: Vec3::NEG_Y * 0.61,
										..default()
									},
									..default()
								}
							),
					),
					(
						Name::new("quit_btn"),
						Button3dBundle {
							shape: WidgetShape(SharedShape::cuboid(2.2, 0.6, 0.6)),
							mesh: meshes.add(Cuboid::new(4.4, 1.2, 1.2)),
							material: mats.add(UiMatBuilder {
								std: StandardMaterial {
									base_color: Color::BLACK,
									emissive: Color::ORANGE_RED * 16.0,
									..default()
								},
								..default()
							}),
							handlers: vec![
								dbg_event(),
								on_ok(|cmds| {
									cmds.commands().add(|world: &mut World| {
										world.resource_mut::<Events<AppExit>>()
											.send(AppExit);
									});
									ControlFlow::Break(())
								}),
							].into(),
							..default()
						},
						AdjacentWidgets::vertical_siblings(),
						=> |cmds| {
							cmds.set_enum(pause_menu_widget::QuitButton);
						};
						#children:
							(
								Name::new("quit_btn_text"),
								Text3dBundle {
									text_3d: Text3d { text: "Exit Game".into(), ..default() },
									font: ui_fonts.mono_3d.clone(),
									material: btn_txt_mat.clone(),
									transform: Transform {
										translation: Vec3::NEG_Y * 0.61,
										..default()
									},
									..default()
								}
							),
					),
			),
	))
	.with_enum(pause_menu_widget::Panel);
}

#[derive(EnumComponent)]
#[component(mutable, derive(Debug, PartialEq, Eq))]
pub enum PauseMenuWidget {
	Panel,
	ResumeButton,
	SettingsButton,
	QuitButton,
}

pub fn show_pause_menu(
	mut cmds: Commands,
	stack: Query<&MenuStack, With<GlobalUi>>,
	panel: Query<Entity, WithVariant<pause_menu_widget::Panel>>,
	actions_q: Query<&ActionState<PlayerAction>>,
	states: Res<StateStack<InputState>>,
	systems: Res<PauseSystems>,
) {
	let id = panel.single();
	for actions in &actions_q {
		if actions.just_pressed(&PlayerAction::PauseGame) {
			let mut stack = stack.single();
			if stack.last() == Some(&id) {
				cmds.run_system(systems.unpause);
			} else if states.last() == Some(&InputState::InGame) {
				cmds.run_system(systems.pause);
			}
		}
	}
}

pub fn pause(
	mut cmds: Commands,
	mut stack: Query<&mut MenuStack, With<GlobalUi>>,
	mut cam: Query<&mut UiCam, With<GlobalUi>>,
	panel: Query<Entity, WithVariant<pause_menu_widget::Panel>>,
	resume_btn: Query<Entity, WithVariant<pause_menu_widget::ResumeButton>>,
	mut states: ResMut<StateStack<InputState>>,
) {
	let id = panel.single();
	let mut stack = stack.single_mut();
	let mut cam = cam.single_mut();
	if states.last() == Some(&InputState::InGame) {
		cmds.entity(id).fade_in_secs(0.5);
		states.push(InputState::InMenu);
		stack.push(id);
		cam.focus = Some(resume_btn.single());
	} else {
		error!("Can't pause game while it's not running");
	}
}

pub fn unpause(
	mut cmds: Commands,
	mut stack: Query<&mut MenuStack, With<GlobalUi>>,
	mut cam: Query<&mut UiCam, With<GlobalUi>>,
	panel: Query<Entity, WithVariant<pause_menu_widget::Panel>>,
	mut states: ResMut<StateStack<InputState>>,
) {
	let id = panel.single();
	let mut stack = stack.single_mut();
	let mut cam = cam.single_mut();
	if stack.last() == Some(&id) {
		debug_assert_eq!(states.last(), Some(&InputState::InMenu));
		cmds.entity(id).fade_out_secs(0.5);
		states.pop();
		stack.pop();
		cam.focus = stack.last().copied();
	} else {
		error!("Pause menu isn't the top of the MenuStack");
	}
}
