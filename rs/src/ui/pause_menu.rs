use super::*;
use crate::settings::Settings;
use bevy::app::AppExit;
use bevy_pkv::PkvStore;
use bevy_quickmenu::{ActionTrait, Menu, MenuItem, MenuState, QuickMenuPlugin, ScreenTrait};

pub fn plugin(app: &mut App) -> &mut App {
	app.add_event::<PauseMenuAction>()
		.add_plugins(QuickMenuPlugin::<PauseMenuScreen>::new())
		.add_systems(Startup, pause_menu_setup)
		.add_systems(Update, event_reader)
}

pub fn pause_menu_setup(mut _cmds: Commands) {
	// spawn_pause_menu(&mut cmds, Settings::default())
}

pub fn spawn_pause_menu(cmds: &mut Commands, state: Settings) {
	cmds.insert_resource(MenuState::new(state, PauseMenuScreen::Root, None));
}

#[derive(Event, Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum PauseMenuAction {
	ShowOrHide,
	// SetBloom(bool),
	SetMsaa(bool),
	SetFxaa(bool),
	Quit,
}

fn event_reader(
	mut cmds: Commands,
	mut events: EventReader<PauseMenuAction>,
	state: Option<Res<MenuState<PauseMenuScreen>>>,
	mut exit_events: EventWriter<AppExit>,
	mut store: ResMut<PkvStore>,
	mut settings: ResMut<Settings>,
) {
	for e in events.iter() {
		match e {
			PauseMenuAction::ShowOrHide => {
				if state.is_none() {
					spawn_pause_menu(&mut cmds, settings.clone())
				} else {
					bevy_quickmenu::cleanup(&mut cmds)
				}
			}
			// PauseMenuAction::SetBloom(on) => {
			// 	info!("Turning bloom {on}");
			// 	settings.hdr = *on;
			// 	store.set("hdr", on).unwrap()
			// }
			PauseMenuAction::SetMsaa(on) => {
				info!("Turning MSAA {on}");
				settings.msaa = *on;
				store.set("msaa", on).unwrap()
			}
			PauseMenuAction::SetFxaa(on) => {
				info!("Turning FXAA {on}");
				settings.fxaa = *on;
				store.set("fxaa", on).unwrap()
			}
			PauseMenuAction::Quit => exit_events.send(AppExit),
		}
	}
}

impl ActionTrait for PauseMenuAction {
	type State = Settings;
	type Event = Self;

	fn handle(&self, state: &mut Self::State, events: &mut EventWriter<Self::Event>) {
		match self {
			PauseMenuAction::ShowOrHide => events.send(PauseMenuAction::ShowOrHide),
			// PauseMenuAction::SetBloom(on) => {
			// 	state.hdr = *on;
			// 	events.send(PauseMenuAction::SetBloom(*on));
			// }
			PauseMenuAction::SetMsaa(on) => {
				state.msaa = *on;
				events.send(PauseMenuAction::SetMsaa(*on));
			}
			PauseMenuAction::SetFxaa(on) => {
				state.fxaa = *on;
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
	type State = Settings;

	fn resolve(&self, state: &<<Self as ScreenTrait>::Action as ActionTrait>::State) -> Menu<Self> {
		match self {
			PauseMenuScreen::Root => root_menu(state),
		}
	}
}

fn root_menu(state: &Settings) -> Menu<PauseMenuScreen> {
	Menu::new(
		"root",
		vec![
			MenuItem::headline("Menu"),
			MenuItem::label("Graphics"),
			// MenuItem::action("Bloom Lighting", PauseMenuAction::SetBloom(!state.hdr))
			// 	.checked(state.hdr),
			MenuItem::action("MSAA", PauseMenuAction::SetMsaa(!state.msaa)).checked(state.msaa),
			MenuItem::action("FXAA", PauseMenuAction::SetFxaa(!state.fxaa)).checked(state.fxaa),
			MenuItem::action("Quit", PauseMenuAction::Quit),
		],
	)
}
