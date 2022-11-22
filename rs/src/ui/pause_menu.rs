use bevy::app::AppExit;
use bevy::prelude::*;
use bevy_quickmenu::{ActionTrait, Menu, MenuItem, MenuState, QuickMenuPlugin, ScreenTrait};
use enum_components::EntityEnumCommands;
use super::*;

pub fn plugin(app: &mut App) -> &mut App {
	app
		.add_event::<PauseMenuEvent>()
		.add_plugin(QuickMenuPlugin::<PauseMenuState, PauseMenuAction, PauseMenuScreen>::new())
		.add_startup_system(pause_menu_setup)
		.add_system(event_reader)
		.add_system(toggle_pause_menu)
}

pub fn pause_menu_setup(mut cmds: Commands) {
	spawn_pause_menu(&mut cmds, PauseMenuState::default())
}

pub fn spawn_pause_menu(cmds: &mut Commands, state: PauseMenuState) {
	cmds.spawn((
		VisibilityBundle::default(),
		PauseMenuState::default(),
	)).set_enum(GameMenu::PauseMenu);
	cmds.insert_resource(MenuState::new(
		state,
		PauseMenuScreen::Root,
		None,
	));
}

pub fn toggle_pause_menu(
	input: Res<Input<KeyCode>>,
	mut events: EventWriter<PauseMenuEvent>,
	state: Option<Res<MenuState<PauseMenuState, PauseMenuAction, PauseMenuScreen>>>,
) {
	if input.just_pressed(KeyCode::Escape) {
		if state.is_some() {
			events.send(PauseMenuEvent::Close)
		} else {
			events.send(PauseMenuEvent::Open)
		}
	}
}


#[derive(Component, Default, Debug)]
pub struct PauseMenuState {
	bloom_on: bool,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum PauseMenuAction {
	Close,
	SetBloom(bool),
	Quit,
}

fn event_reader(
	mut cmds: Commands,
	mut events: EventReader<PauseMenuEvent>,
	mut exit_events: EventWriter<AppExit>,
	mut cam_q: Query<&mut Camera>,
) {
	for e in events.iter() {
		match e {
			PauseMenuEvent::Open => spawn_pause_menu(&mut cmds, PauseMenuState {
				bloom_on: cam_q.iter().next().unwrap().hdr
			}),
			PauseMenuEvent::Close => bevy_quickmenu::cleanup(&mut cmds),
			PauseMenuEvent::SetBloom(on) => {
				info!("Turning bloom {on}");
				for mut cam in &mut cam_q {
					cam.hdr = *on;
				}
			},
			PauseMenuEvent::Quit => exit_events.send(AppExit),
		}
	}
}

impl ActionTrait for PauseMenuAction {
	type State = PauseMenuState;
	type Event = PauseMenuEvent;
	
	fn handle(&self, state: &mut Self::State, events: &mut EventWriter<Self::Event>) {
		match self {
			PauseMenuAction::Close => events.send(PauseMenuEvent::Close),
			PauseMenuAction::SetBloom(on) => {
				state.bloom_on = *on;
				events.send(PauseMenuEvent::SetBloom(*on))
			},
			PauseMenuAction::Quit => events.send(PauseMenuEvent::Quit)
		}
	}
}


#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum PauseMenuScreen {
	Root,
}

impl ScreenTrait for PauseMenuScreen {
	type Action = PauseMenuAction;
	
	fn resolve(&self, state: &<<Self as ScreenTrait>::Action as ActionTrait>::State) -> Menu<Self::Action, Self, <<Self as ScreenTrait>::Action as ActionTrait>::State> {
		match self {
			PauseMenuScreen::Root => root_menu(state),
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub enum PauseMenuEvent {
	Open,
	Close,
	SetBloom(bool),
	Quit,
}

fn root_menu(state: &PauseMenuState) -> Menu<PauseMenuAction, PauseMenuScreen, PauseMenuState> {
	Menu::new(
		"root",
		vec![
			MenuItem::headline("Pause"),
			MenuItem::label("Toggle Bloom"),
			MenuItem::action("BloomOn", PauseMenuAction::SetBloom(true))
				.checked(state.bloom_on),
			MenuItem::action("BloomOff", PauseMenuAction::SetBloom(false))
				.checked(!state.bloom_on),
			MenuItem::action("Quit", PauseMenuAction::Quit)
		]
	)
}
