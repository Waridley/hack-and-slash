use crate::{
	player::input::PlayerAction,
	ui::{
		settings_menu,
		settings_menu::{settings_sub_menu, SettingsMenu, SettingsSubMenus},
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
	window::CursorGrabMode,
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
		text::UiFonts,
		widgets::{
			dbg_event, focus_state_colors, new_unlit_material, on_action, on_ok, CuboidFaces,
			CuboidPanel, CuboidPanelBundle, InteractHandlers, Node3dBundle, PanelBundle,
			RectCorners, Text3d, Text3dBundle, WidgetBundle, WidgetShape,
		},
		Fade, FadeCommands, GlobalUi, MenuRef, MenuStack, UiCam, UiMat, UiMatBuilder,
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
				std: Color::rgba(0.3, 0.3, 0.3, 0.3).into(),
				..default()
			}),
			adjacent: AdjacentWidgets::all(FocusTarget::ChildN(0)),
			..default()
		},
		Fade::ZERO;
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
					PlanarPolyLine::rect(12.0, 12.0, 0.5).flat()
				),
				mats.add(UiMatBuilder::from(Color::rgba(0.12, 0.004, 0.15, 0.92)))
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
					font: ui_fonts.mono.clone(),
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
					shape: WidgetShape { shape: SharedShape::cuboid(5.5, 1.0, 5.5), ..default() },
					transform: Transform {
						translation: Vec3::NEG_Y * 6.5,
						..default()
					},
					adjacent: AdjacentWidgets::all(FocusTarget::ChildN(0)),
					..default()
				},
				LineUpChildren::vertical().with_spacing(0.1);
				#children:
					(
						Name::new("resume_btn"),
						PanelBundle {
							shape: WidgetShape { shape: SharedShape::cuboid(3.5, 0.6, 0.6), ..default() },
							mesh: meshes.add(Cuboid::new(7.0, 1.2, 1.2)),
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
								focus_state_colors(Color::BLACK, Color::LIME_GREEN),
							].into(),
							adjacent: AdjacentWidgets::vertical_siblings(),
							..default()
						},
						=> |cmds| {
							cmds.set_enum(pause_menu_widget::ResumeButton);
						};
						#children: (
							Name::new("resume_btn_text"),
							Text3dBundle {
								text_3d: Text3d { text: "Resume Game".into(), ..default() },
								font: ui_fonts.mono.clone(),
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
						PanelBundle {
							shape: WidgetShape { shape: SharedShape::cuboid(3.5, 0.6, 0.6), ..default() },
							mesh: meshes.add(Cuboid::new(7.0, 1.2, 1.2)),
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
										world.entity_mut(panel_id).fade_in_secs(1.5);

										let top_menu = world.resource::<SettingsSubMenus>().top;
										let mut q = world.query_filtered::<&mut MenuStack, With<GlobalUi>>();
										let mut stack = q.single_mut(world);
										stack.push(top_menu);
									});
									ControlFlow::Break(())
								}),
								focus_state_colors(Color::BLACK, Color::GRAY),
							].into(),
							adjacent: AdjacentWidgets::vertical_siblings(),
							..default()
						},
						=> |cmds| {
							cmds.set_enum(pause_menu_widget::SettingsButton);
						};
						#children:
							(
								Name::new("settings_btn_text"),
								Text3dBundle {
									text_3d: Text3d { text: "Settings...".into(), ..default() },
									font: ui_fonts.mono.clone(),
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
						PanelBundle {
							shape: WidgetShape { shape: SharedShape::cuboid(3.0, 0.6, 0.6), ..default() },
							mesh: meshes.add(Cuboid::new(6.0, 1.2, 1.2)),
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
								focus_state_colors(Color::BLACK, Color::ORANGE_RED),
							].into(),
							adjacent: AdjacentWidgets::vertical_siblings(),
							..default()
						},
						=> |cmds| {
							cmds.set_enum(pause_menu_widget::QuitButton);
						};
						#children:
							(
								Name::new("quit_btn_text"),
								Text3dBundle {
									text_3d: Text3d { text: "Quit Game".into(), ..default() },
									font: ui_fonts.mono.clone(),
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
			if stack.last().map(|menu| menu.root) == Some(id) {
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
	panel: Query<Entity, WithVariant<pause_menu_widget::Panel>>,
	resume_btn: Query<Entity, WithVariant<pause_menu_widget::ResumeButton>>,
	mut states: ResMut<StateStack<InputState>>,
	mut windows: Query<&mut Window>,
) {
	let Ok(mut window) = windows.get_single_mut() else {
		// probably exiting if window is missing
		return;
	};

	let id = panel.single();
	let mut stack = stack.single_mut();
	if states.last() == Some(&InputState::InGame) {
		cmds.entity(id).fade_in_secs(0.5);
		states.push(InputState::InMenu);
		stack.push(MenuRef {
			focus: resume_btn.single(),
			..MenuRef::new(id)
		});
		window.cursor.visible = true;
		window.cursor.grab_mode = CursorGrabMode::None;
	} else {
		error!("Can't pause game while it's not running");
	}
}

pub fn unpause(
	mut cmds: Commands,
	mut stack: Query<&mut MenuStack, With<GlobalUi>>,
	panel: Query<Entity, WithVariant<pause_menu_widget::Panel>>,
	mut states: ResMut<StateStack<InputState>>,
	mut windows: Query<&mut Window>,
) {
	let Ok(mut window) = windows.get_single_mut() else {
		// probably exiting if window is missing
		return;
	};
	let id = panel.single();
	let mut stack = stack.single_mut();
	if stack.last().map(|menu| menu.root) == Some(id) {
		debug_assert_eq!(states.last(), Some(&InputState::InMenu));
		cmds.entity(id).fade_out_secs(0.5);
		states.pop();
		stack.pop();
		window.cursor.visible = false;
		window.cursor.grab_mode = CursorGrabMode::Locked;
	} else {
		error!("Pause menu isn't the top of the MenuStack");
	}
}
