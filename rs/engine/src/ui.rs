use bevy::{
	app::{App, Update},
	ecs::schedule::SystemConfigs,
	input::common_conditions::input_toggle_active,
	prelude::{
		Condition, Deref, DerefMut, IntoSystemConfigs, KeyCode, Plugin, Reflect, ResMut, Resource,
		SystemSet,
	},
};

#[cfg(feature = "debugging")]
pub mod dbg;

pub struct UiPlugin;

impl Plugin for UiPlugin {
	fn build(&self, app: &mut App) {
		#[cfg(feature = "debugging")]
		app.add_plugins(dbg::DebugUiPlugin);

		app.init_resource::<UiHovered>()
			.add_systems(Update, reset_hovered);
	}
}

/// Keeps track of whether a UI element is hovered over so that clicking
/// does not grab the mouse if so.
#[derive(Resource, Debug, Default, Copy, Clone, Deref, DerefMut, Reflect)]
pub struct UiHovered(bool);

#[derive(SystemSet, Copy, Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ShowDebugWindows;

pub trait AddDebugUi {
	fn add_debug_systems<M>(&mut self, systems: impl IntoSystemConfigs<M>) -> &mut Self;
}

impl AddDebugUi for App {
	fn add_debug_systems<M>(&mut self, _systems: impl IntoSystemConfigs<M>) -> &mut Self {
		#[cfg(feature = "debugging")]
		self.add_systems(Update, _systems.in_set(ShowDebugWindows));
		self
	}
}

pub trait ToggleUi<M> {
	/// Debug UI window will be hidden by default, and can be shown by pressing the given
	/// key while the debug interface is visible.
	fn show_with(self, key: KeyCode) -> SystemConfigs;
	/// Debug UI window will be visible by default, and can be hidden by pressing the given
	/// key while the debug interface is visible.
	fn hide_with(self, key: KeyCode) -> SystemConfigs;
}

impl<S: IntoSystemConfigs<M>, M> ToggleUi<M> for S {
	fn show_with(self, key: KeyCode) -> SystemConfigs {
		self.run_if(dbg_window_toggled(false, key))
	}

	fn hide_with(self, key: KeyCode) -> SystemConfigs {
		self.run_if(dbg_window_toggled(true, key))
	}
}

pub fn dbg_window_toggled(default: bool, code: KeyCode) -> impl Condition<()> {
	input_toggle_active(false, KeyCode::Grave).and_then(input_toggle_active(default, code))
}

pub fn reset_hovered(mut ui_hovered: ResMut<UiHovered>) {
	if **ui_hovered {
		**ui_hovered = false;
	}
}
