use crate::{
	player::input::PlayerAction,
	ui::{
		prefs_menu::PrefsMenu,
		settings_menu::{SettingsMenu, SettingsSubMenus},
	},
};
use bevy::{
	app::AppExit,
	color::palettes::{
		basic::{GRAY, TEAL},
		css::{LIMEGREEN, ORANGE_RED},
	},
	ecs::system::{RunSystemOnce, SystemId},
	prelude::*,
	window::CursorGrabMode,
};
use engine::{
	entity_tree,
	input::InputState,
	ui::{
		focus::{AdjacentWidgets, FocusTarget},
		layout::{ExpandToFitChildren, LineUpChildren},
		text::UiFonts,
		widgets::{
			borders::Border, dbg_event, focus_state_colors, focus_state_emissive, on_ok,
			CuboidPanel, InteractHandlers, Node3d, Text3d, WidgetShape,
		},
		Fade, FadeCommands, GlobalUi, MenuRef, MenuStack, UiMat, UiMatBuilder,
	},
	util::StateStack,
};
use enum_components::{EntityEnumCommands, EnumComponent, WithVariant};
use leafwing_input_manager::action_state::ActionState;
use rapier3d::geometry::SharedShape;
use smallvec::smallvec;
use std::ops::ControlFlow;
use tiny_bail::prelude::r;

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

pub fn setup(mut cmds: Commands, mut mats: ResMut<Assets<UiMat>>, ui_fonts: Res<UiFonts>) {
	let btn_txt_mat = MeshMaterial3d(mats.add(UiMatBuilder {
		std: StandardMaterial {
			base_color: Color::BLACK,
			unlit: true,
			..default()
		},
		..default()
	}));

	let expand = ExpandToFitChildren {
		margin: Vec3::new(0.2, 0.0, 0.3),
		offset: Vec3::new(0.0, 0.51, 0.0),
		..default()
	};

	entity_tree!(cmds; (
		Name::new("PauseMenu"),
		CuboidPanel {
			size: Vec3::new(12.0, 12.0, 12.0),
			..default()
		},
		MeshMaterial3d(mats.add(UiMatBuilder {
			std: Color::srgba(0.3, 0.3, 0.3, 0.3).into(),
			..default()
		})),
		AdjacentWidgets::all(FocusTarget::ChildN(0)),
		Fade::ZERO;
		#children: [
			(
				Name::new("border"),
				Node3d,
				Transform {
					translation: Vec3::NEG_Y * 6.5,
					..default()
				},
				Border::default(),
				MeshMaterial3d(mats.add(UiMatBuilder::from(Color::srgba(0.12, 0.004, 0.15, 0.92))))
			),
			(
				Name::new("game_paused_txt"),
				Text3d {
					text: "Game Paused".into(),
					font: ui_fonts.mono.clone(),
					flat: false,
					..default()
				},
				MeshMaterial3d(mats.add(UiMatBuilder::from(Color::WHITE))),
				Transform {
					translation: Vec3::new(0.0, -6.5, 5.0),
					..default()
				},
			),
			(
				Name::new("pause_menu_btns_container"),
				WidgetShape { shape: SharedShape::cuboid(5.5, 1.0, 5.5), ..default() },
				Transform {
					translation: Vec3::NEG_Y * 6.5,
					..default()
				},
				AdjacentWidgets::all(FocusTarget::ChildN(0)),
				LineUpChildren::vertical().with_spacing(0.2);
				#children: [
					(
						Name::new("resume_btn"),
						CuboidPanel::default(),
						MeshMaterial3d(mats.add(UiMatBuilder {
							std: StandardMaterial {
								base_color: Color::BLACK,
								emissive: LinearRgba::from(LIMEGREEN) * 3.0,
								..default()
							},
							..default()
						})),
						InteractHandlers(smallvec![
							dbg_event(),
							on_ok(|cmds| {
								cmds.commands().queue(|world: &mut World| {
									r!(world.run_system_once(unpause));
								});
								ControlFlow::Break(())
							}),
							focus_state_colors(Color::BLACK, Color::from(LIMEGREEN)),
						]),
						AdjacentWidgets::vertical_siblings(),
						expand.clone(),
						;
						=> |cmds| {
							cmds.set_enum(pause_menu_widget::ResumeButton);
						}
						#children: [(
							Name::new("resume_btn_txt"),
							Text3d {
								text: "Resume Game".into(),
								font: ui_fonts.mono.clone(),
								..default()
							},
							btn_txt_mat.clone(),
							Transform {
								translation: Vec3::NEG_Y * 0.61,
								..default()
							},
						)]
					),
					(
						Name::new("prefs_btn"),
						CuboidPanel::default(),
						MeshMaterial3d(mats.add(UiMatBuilder {
							std: StandardMaterial {
								base_color: Color::BLACK,
								emissive: LinearRgba::from(TEAL) * 4.0,
								..default()
							},
							..default()
						})),
						InteractHandlers(smallvec![
							dbg_event(),
							on_ok(|cmds| {
								cmds.commands().queue(|world: &mut World| {
									let mut q = world.query_filtered::<Entity, With<PrefsMenu>>();
									let panel_id = q.single(world);
									world.entity_mut(panel_id).fade_in_secs(0.5);

									let top_menu = MenuRef::new(panel_id);
									let mut q = world.query_filtered::<&mut MenuStack, With<GlobalUi>>();
									let mut stack = q.single_mut(world);
									stack.push(top_menu);
								});
								ControlFlow::Break(())
							}),
							focus_state_emissive(LinearRgba::from(TEAL) * 4.0, LinearRgba::from(TEAL) * 6.0),
						]),
						AdjacentWidgets::vertical_siblings(),
						expand.clone(),
						;
						=> |cmds| {
							cmds.set_enum(pause_menu_widget::PrefsButton);
						}
						#children: [
							(
								Name::new("prefs_btn_txt"),
								Text3d {
									text: "Preferences...".into(),
									font: ui_fonts.mono.clone(),
									..default()
								},
								btn_txt_mat.clone(),
								Transform {
									translation: Vec3::NEG_Y * 0.61,
									..default()
								},
							),
						]
					),
					(
						Name::new("settings_btn"),
						CuboidPanel::default(),
						MeshMaterial3d(mats.add(UiMatBuilder {
							std: StandardMaterial {
								base_color: Color::BLACK,
								emissive: LinearRgba::gray(0.5),
								..default()
							},
							..default()
						})),
						InteractHandlers(smallvec![
							dbg_event(),
							on_ok(|cmds| {
								cmds.commands().queue(|world: &mut World| {
									let mut q = world.query_filtered::<Entity, With<SettingsMenu>>();
									let panel_id = q.single(world);
									world.entity_mut(panel_id).fade_in_secs(0.5);

									let top_menu = world.resource::<SettingsSubMenus>().top;
									let mut q = world.query_filtered::<&mut MenuStack, With<GlobalUi>>();
									let mut stack = q.single_mut(world);
									stack.push(top_menu);
								});
								ControlFlow::Break(())
							}),
							focus_state_colors(Color::BLACK, Color::from(GRAY)),
						]),
						AdjacentWidgets::vertical_siblings(),
						expand.clone(),
						;
						=> |cmds| {
							cmds.set_enum(pause_menu_widget::SettingsButton);
						}
						#children: [
							(
								Name::new("settings_btn_txt"),
								Text3d {
									text: "Settings...".into(),
									font: ui_fonts.mono.clone(),
									..default()
								},
								btn_txt_mat.clone(),
								Transform {
									translation: Vec3::NEG_Y * 0.61,
									..default()
								},
							),
						]
					),
					(
						Name::new("quit_btn"),
						CuboidPanel::default(),
						MeshMaterial3d(mats.add(UiMatBuilder {
							std: StandardMaterial {
								base_color: Color::BLACK,
								emissive: LinearRgba::from(ORANGE_RED) * 3.0,
								..default()
							},
							..default()
						})),
						InteractHandlers(smallvec![
							dbg_event(),
							on_ok(|cmds| {
								cmds.commands().queue(|world: &mut World| {
									world.resource_mut::<Events<AppExit>>()
										.send(AppExit::Success);
								});
								ControlFlow::Break(())
							}),
							focus_state_colors(Color::BLACK, Color::from(ORANGE_RED)),
						]),
						AdjacentWidgets::vertical_siblings(),
						expand.clone(),
						;
						=> |cmds| {
							cmds.set_enum(pause_menu_widget::QuitButton);
						}
						#children: [
							(
								Name::new("quit_btn_txt"),
								Text3d {
									text: "Quit Game".into(),
									font: ui_fonts.mono.clone(),
									..default()
								},
								btn_txt_mat.clone(),
								Transform {
									translation: Vec3::NEG_Y * 0.61,
									..default()
								},
							),
						]
					),
				]
			),
		]
	))
	.with_enum(pause_menu_widget::Panel);
}

#[derive(EnumComponent)]
#[component(mutable, derive(Debug, PartialEq, Eq))]
pub enum PauseMenuWidget {
	Panel,
	ResumeButton,
	SettingsButton,
	PrefsButton,
	QuitButton,
}

pub fn show_pause_menu(
	mut cmds: Commands,
	stack: Single<&MenuStack, With<GlobalUi>>,
	panel: Single<Entity, WithVariant<pause_menu_widget::Panel>>,
	actions_q: Query<&ActionState<PlayerAction>>,
	states: Res<StateStack<InputState>>,
	systems: Res<PauseSystems>,
) {
	for actions in &actions_q {
		if actions.just_pressed(&PlayerAction::PauseGame) {
			if stack.last().map(|menu| menu.root) == Some(*panel) {
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
		window.cursor_options.visible = true;
		window.cursor_options.grab_mode = CursorGrabMode::None;
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
		window.cursor_options.visible = false;
		window.cursor_options.grab_mode = CursorGrabMode::Locked;
	} else {
		error!("Pause menu isn't the top of the MenuStack");
	}
}
