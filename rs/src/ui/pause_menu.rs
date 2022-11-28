use super::*;
use bevy::app::AppExit;
use bevy::core_pipeline::fxaa::Fxaa;
use bevy_quickmenu::{ActionTrait, Menu, MenuItem, MenuState, QuickMenuPlugin, ScreenTrait};

pub fn plugin(app: &mut App) -> &mut App {
	app.add_event::<PauseMenuAction>()
		.add_plugin(QuickMenuPlugin::<
			PauseMenuState,
			PauseMenuAction,
			PauseMenuScreen,
		>::new())
		.add_startup_system(pause_menu_setup)
		.add_system(event_reader)
}

pub fn pause_menu_setup(mut _cmds: Commands) {
	// spawn_pause_menu(&mut cmds, PauseMenuState::default())
}

pub fn spawn_pause_menu(cmds: &mut Commands, state: PauseMenuState) {
	cmds.insert_resource(MenuState::new(state, PauseMenuScreen::Root, None));
}

#[derive(Component, Default, Debug)]
pub struct PauseMenuState {
	bloom_on: bool,
	msaa_on: bool,
	fxaa_on: bool,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum PauseMenuAction {
	ShowOrHide,
	SetBloom(bool),
	SetMsaa(bool),
	SetFxaa(bool),
	Quit,
}

fn event_reader(
	mut cmds: Commands,
	mut events: EventReader<PauseMenuAction>,
	state: Option<Res<MenuState<PauseMenuState, PauseMenuAction, PauseMenuScreen>>>,
	mut exit_events: EventWriter<AppExit>,
	mut cam_q: Query<(&mut Camera, &mut Fxaa)>,
	mut msaa: ResMut<Msaa>,
) {
	for e in events.iter() {
		match e {
			PauseMenuAction::ShowOrHide => {
				if state.is_none() {
					spawn_pause_menu(
						&mut cmds,
						PauseMenuState {
							bloom_on: cam_q.iter().next().unwrap().0.hdr,
							msaa_on: msaa.samples > 1,
							fxaa_on: cam_q.iter().next().unwrap().1.enabled,
						},
					)
				} else {
					bevy_quickmenu::cleanup(&mut cmds)
				}
			}
			PauseMenuAction::SetBloom(on) => {
				info!("Turning bloom {on}");
				for (mut cam, _) in &mut cam_q {
					cam.hdr = *on;
				}
			}
			PauseMenuAction::SetMsaa(on) => {
				info!("Turning MSAA {on}");
				msaa.samples = if *on { 4 } else { 1 }
			}
			PauseMenuAction::SetFxaa(on) => {
				info!("Turning FXAA {on}");
				for (_, mut fxaa) in &mut cam_q {
					fxaa.enabled = *on
				}
			}
			PauseMenuAction::Quit => exit_events.send(AppExit),
		}
	}
}

impl ActionTrait for PauseMenuAction {
	type State = PauseMenuState;
	type Event = PauseMenuAction;

	fn handle(&self, state: &mut Self::State, events: &mut EventWriter<Self::Event>) {
		match self {
			PauseMenuAction::ShowOrHide => events.send(PauseMenuAction::ShowOrHide),
			PauseMenuAction::SetBloom(on) => {
				state.bloom_on = *on;
				events.send(PauseMenuAction::SetBloom(*on));
				#[cfg(target_family = "wasm")]
				if *on {
					state.msaa_on = false;
					events.send(PauseMenuAction::SetMsaa(false))
				}
			}
			PauseMenuAction::SetMsaa(on) => {
				state.msaa_on = *on;
				events.send(PauseMenuAction::SetMsaa(*on));
				#[cfg(target_family = "wasm")]
				if *on {
					state.bloom_on = false;
					events.send(PauseMenuAction::SetBloom(false))
				}
			}
			PauseMenuAction::SetFxaa(on) => {
				state.fxaa_on = *on;
				events.send(PauseMenuAction::SetFxaa(*on));
			}
			PauseMenuAction::Quit => events.send(PauseMenuAction::Quit),
		}
	}
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum PauseMenuScreen {
	Root,
}

impl ScreenTrait for PauseMenuScreen {
	type Action = PauseMenuAction;

	fn resolve(
		&self,
		state: &<<Self as ScreenTrait>::Action as ActionTrait>::State,
	) -> Menu<Self::Action, Self, <<Self as ScreenTrait>::Action as ActionTrait>::State> {
		match self {
			PauseMenuScreen::Root => root_menu(state),
		}
	}
}

fn root_menu(state: &PauseMenuState) -> Menu<PauseMenuAction, PauseMenuScreen, PauseMenuState> {
	Menu::new(
		"root",
		vec![
			MenuItem::headline("Menu"),
			MenuItem::label("Graphics"),
			MenuItem::action("Bloom Lighting", PauseMenuAction::SetBloom(!state.bloom_on))
				.checked(state.bloom_on),
			MenuItem::action("MSAA", PauseMenuAction::SetMsaa(!state.msaa_on))
				.checked(state.msaa_on),
			MenuItem::action("FXAA", PauseMenuAction::SetFxaa(!state.fxaa_on))
				.checked(state.fxaa_on),
			MenuItem::action("Quit", PauseMenuAction::Quit),
		],
	)
}
