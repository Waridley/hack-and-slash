use crate::player::input::PlayerAction;
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
	entity_tree,
	input::InputState,
	mats::{
		fade::DitherFade,
		fog::{DistanceDither, Matter},
		ExtMat,
	},
	ui::{
		focus::{AdjacentWidgets, FocusTarget, Wedge2d},
		layout::LineUpChildren,
		widgets::{
			dbg_event, new_unlit_material, on_ok, Button3dBundle, CuboidFaces, CuboidPanel,
			CuboidPanelBundle, InteractHandlers, RectBorderDesc, RectCorners, Text3d, Text3dBundle,
			WidgetBundle, WidgetShape,
		},
		Fade, FadeCommands, GlobalUi, MenuStack, UiCam, UiFonts, UiMat, UiMatBuilder,
	},
	util::StateStack,
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
	let material = mats.add(UiMatBuilder {
		std: Color::rgba(0.5, 0.5, 0.5, 0.5).into(),
		..default()
	});
	let pause_menu = entity_tree!(cmds; (
		Name::new("PauseMenu"),
		CuboidPanelBundle {
			panel: CuboidPanel::<UiMat> {
				size: Vec3::new(12.0, 12.0, 12.0),
				borders: CuboidFaces {
					front: vec![RectBorderDesc {
						colors: Some(RectCorners {
							top_right: Color::WHITE,
							top_left: Color::GREEN,
							bottom_left: Color::BLUE,
							bottom_right: Color::RED,
						}),
						material: mats.add(UiMatBuilder::default()),
						..default()
					}],
					..default()
				},
				..default()
			},
			material,
			..default()
		},
		Fade::ZERO,
		AdjacentWidgets::all(FocusTarget::Child(0));
		#children:
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
						Name::new("game_paused_text"),
						Text3dBundle {
							text_3d: Text3d {
								text: "Game Paused".into(),
								..default()
							},
							material: mats.add(new_unlit_material()),
							font: ui_fonts.mono_3d.clone(),
							..default()
						},
						AdjacentWidgets::vertical_siblings(),
					),
					(
						Name::new("resume_button"),
						Button3dBundle {
							shape: WidgetShape(SharedShape::cuboid(3.0, 0.5, 0.5)),
							mesh: meshes.add(Cuboid::new(6.0, 1.0, 1.0)),
							material: mats.add(UiMatBuilder {
								std: StandardMaterial {
									base_color: Color::BLACK,
									emissive: Color::LIME_GREEN * 16.0,
									..default()
								},
								..default()
							}),
							handlers: InteractHandlers(vec![
								dbg_event(),
								on_ok(|cmds| {
									cmds.commands().add(|world: &mut World| {
										world.run_system_once(unpause);
									});
									ControlFlow::Break(())
								}),
							]),
							..default()
						},
						AdjacentWidgets::vertical_siblings()
						=> |cmds| {
							cmds.set_enum(pause_menu_widget::ResumeButton);
						};
						#children: (
							Name::new("resume_button_text"),
							Text3dBundle {
								text_3d: Text3d { text: "Resume Game".into(), ..default() },
								font: ui_fonts.mono_3d.clone(),
								material: mats.add(UiMatBuilder {
									std: StandardMaterial {
										base_color: Color::BLACK,
										unlit: true,
										..default()
									},
									..default()
								}),
								transform: Transform {
									translation: Vec3::NEG_Y,
									..default()
								},
								..default()
							}
						),
					),
					(
						Name::new("quit_button"),
						Button3dBundle {
							shape: WidgetShape(SharedShape::cuboid(2.2, 0.5, 0.5)),
							mesh: meshes.add(Cuboid::new(4.4, 1.0, 1.0)),
							material: mats.add(UiMatBuilder {
								std: StandardMaterial {
									base_color: Color::BLACK,
									emissive: Color::ORANGE_RED * 16.0,
									..default()
								},
								..default()
							}),
							handlers: InteractHandlers(vec![
								dbg_event(),
								on_ok(|cmds| {
									cmds.commands().add(|world: &mut World| {
										world.resource_mut::<Events<AppExit>>()
											.send(AppExit);
									});
									ControlFlow::Break(())
								}),
							]),
							..default()
						},
						AdjacentWidgets::vertical_siblings()
						=> |cmds| {
							cmds.set_enum(pause_menu_widget::QuitButton);
						};
						#children: (
							Name::new("exit_button_text"),
							Text3dBundle {
								text_3d: Text3d { text: "Exit Game".into(), ..default() },
								font: ui_fonts.mono_3d.clone(),
								material: mats.add(UiMatBuilder {
									std: StandardMaterial {
										base_color: Color::BLACK,
										unlit: true,
										..default()
									},
									..default()
								}),
								transform: Transform {
									translation: Vec3::NEG_Y,
									..default()
								},
								..default()
							}
						),
					),
			),
	))
	.with_enum(pause_menu_widget::Panel)
	.id();
}

#[derive(EnumComponent)]
#[component(mutable, derive(Debug, PartialEq, Eq))]
pub enum PauseMenuWidget {
	Panel,
	ResumeButton,
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
