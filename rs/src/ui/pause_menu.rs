use crate::player::input::PlayerAction;
use bevy::{ecs::query::QuerySingleError, prelude::*};
use engine::{
	entity_tree,
	input::InputState,
	mats::{
		fade::DitherFade,
		fog::{DistanceDither, Matter},
		ExtMat,
	},
	ui::{
		layout::LineUpChildren,
		widgets::{
			CuboidPanel, CuboidPanelBundle, Text3d, Text3dBundle, WidgetBundle, WidgetShape,
		},
		Fade, GlobalUi, MenuStack, UiCam, UiFonts, UiMat,
	},
	util::StateStack,
};
use leafwing_input_manager::action_state::ActionState;
use rapier3d::geometry::SharedShape;
use web_time::Duration;
use engine::ui::FadeCommands;
use engine::ui::widgets::new_unlit_material;

pub struct PauseMenuPlugin;

impl Plugin for PauseMenuPlugin {
	fn build(&self, app: &mut App) {
		app.add_systems(Startup, setup)
			.add_systems(Update, show_pause_menu);
	}
}

pub fn setup(mut cmds: Commands, mut mats: ResMut<Assets<UiMat>>, ui_fonts: Res<UiFonts>) {
	let material = mats.add(UiMat {
		extension: default(),
		base: Matter {
			extension: DistanceDither::ui(),
			base: Color::rgba(0.5, 0.5, 0.5, 0.5).into(),
		},
	});
	let pause_menu = entity_tree!(cmds; (
		PauseMenu,
		CuboidPanelBundle {
			panel: CuboidPanel::<UiMat> {
				size: Vec3::new(12.0, 12.0, 12.0),
				..default()
			},
			material,
			..default()
		},
		Fade::ZERO;
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
					Text3dBundle::<ExtMat<DitherFade>> {
						text_3d: Text3d {
							text: "Game Paused".into(),
							..default()
						},
						material: mats.add(new_unlit_material()),
						font: ui_fonts.mono_3d.clone(),
						..default()
					},
				),
			),
	))
	.id();
	cmds.insert_resource(PauseMenuId(pause_menu));
}

#[derive(Resource, Deref, DerefMut, Debug, Copy, Clone, PartialEq, Eq)]
pub struct PauseMenuId(pub Entity);

#[derive(Component, Debug, Reflect)]
#[reflect(Component)]
pub struct PauseMenu;

pub fn show_pause_menu(
	mut cmds: Commands,
	mut stack: Query<&mut MenuStack, With<GlobalUi>>,
	mut cam: Query<&mut UiCam, With<GlobalUi>>,
	id: Res<PauseMenuId>,
	actions_q: Query<&ActionState<PlayerAction>>,
	mut states: ResMut<StateStack<InputState>>,
) {
	let id = **id;
	for actions in &actions_q {
		if actions.just_pressed(&PlayerAction::PauseGame) {
			let mut stack = stack.single_mut();
			let mut cam = cam.single_mut();
			if stack.last() == Some(&id) {
				debug_assert_eq!(states.last(), Some(&InputState::InMenu));
				cmds.entity(id).fade_out_secs(0.5);
				states.pop();
				stack.pop();
				cam.focus = stack.last().copied();
			} else if states.last() == Some(&InputState::InGame) {
				cmds.entity(id).fade_in_secs(0.5);
				states.push(InputState::InMenu);
				stack.push(id);
				cam.focus = Some(id);
			}
		}
	}
}
