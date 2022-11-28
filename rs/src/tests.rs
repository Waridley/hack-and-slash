use bevy::{app::AppExit, prelude::*};

pub fn exit_app(mut events: EventWriter<AppExit>) {
	events.send(AppExit)
}
