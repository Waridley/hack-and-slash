use bevy::ecs::query::QuerySingleError;
use bevy::prelude::*;
use leafwing_input_manager::action_state::ActionState;
use rapier3d::geometry::SharedShape;
use engine::{
	entity_tree,
	ui::widgets::{CuboidPanel, CuboidPanelBundle},
};
use engine::input::InputState;
use engine::ui::{GlobalUi, IsGlobalUiRoot, MenuStack, UiCam, UiFonts};
use engine::ui::layout::LineUpChildren;
use engine::ui::widgets::{Text3d, Text3dBundle, WidgetBundle, WidgetShape};
use engine::util::StateStack;
use crate::player::input::PlayerAction;

pub struct PauseMenuPlugin;

impl Plugin for PauseMenuPlugin {
	fn build(&self, app: &mut App) {
		app.add_systems(Startup, setup)
			.add_systems(Update, show_pause_menu);
	}
}

pub fn setup(
	mut cmds: Commands,
	mut mats: ResMut<Assets<StandardMaterial>>,
	ui_fonts: Res<UiFonts>,
) {
	let material = mats.add(Color::rgba(0.5, 0.5, 0.5, 0.5));
	let pause_menu = entity_tree!(cmds; (
		PauseMenu,
		CuboidPanelBundle {
			panel: CuboidPanel::<StandardMaterial> {
				size: Vec3::new(12.0, 12.0, 12.0),
				..default()
			},
			visibility: Visibility::Hidden,
			material,
			..default()
		};
		#children:
			(
				WidgetBundle {
					shape: WidgetShape(SharedShape::cuboid(5.5, 1.0, 5.5)),
					transform: Transform {
						translation: Vec3::NEG_Y * 6.5,
						..default()
					},
					..default()
				},
				LineUpChildren::vertical();
				#children: (
					Text3dBundle::<StandardMaterial> {
						text_3d: Text3d {
							text: "Game Paused".into(),
							..default()
						},
						font: ui_fonts.mono_3d.clone(),
						..default()
					},
				),
			),
	)).id();
	cmds.insert_resource(PauseMenuId(pause_menu));
}

#[derive(Resource, Deref, DerefMut, Debug, Copy, Clone, PartialEq, Eq)]
pub struct PauseMenuId(pub Entity);

#[derive(Component, Debug, Reflect)]
#[reflect(Component)]
pub struct PauseMenu;

pub fn show_pause_menu(
	mut q: Query<&mut Visibility, With<PauseMenu>>,
	mut stack: Query<&mut MenuStack, With<GlobalUi>>,
	mut cam: Query<&mut UiCam, With<GlobalUi>>,
	id: Res<PauseMenuId>,
	actions_q: Query<&ActionState<PlayerAction>>,
	mut states: ResMut<StateStack<InputState>>,
) {
	for actions in &actions_q {
		if actions.just_pressed(&PlayerAction::PauseGame) {
			let mut vis = match q.get_single_mut() {
				Ok(mut vis) => vis,
				Err(e) => {
					error!("{e}");
					return
				}
			};
			let mut stack = stack.single_mut();
			let mut cam = cam.single_mut();
			if stack.last() == Some(&**id) {
				debug_assert_eq!(states.last(), Some(&InputState::InMenu));
				*vis = Visibility::Hidden;
				states.pop();
				stack.pop();
				cam.focus = stack.last().copied();
			} else if states.last() == Some(&InputState::InGame) {
				*vis = Visibility::Inherited;
				states.push(InputState::InMenu);
				stack.push(**id);
				cam.focus = Some(**id);
			}
		}
	}
}