use async_process::{Child, Command, Stdio};
use bevy::prelude::*;
use bevy::tasks::IoTaskPool;
use futures_lite::{io::BufReader, AsyncBufReadExt, StreamExt};

#[bevy_main]
fn main() {
	App::new()
		.add_plugins(DefaultPlugins.set(AssetPlugin {
			watch_for_changes: true,
			..default()
		}))
		.add_event::<RunGameEvent>()
		.insert_resource(RunningGame::default())
		.add_startup_system(setup)
		.add_system_to_stage(CoreStage::First, clear_running_game)
		.add_system(run_game)
		.add_system(run_game_btn)
		.run()
}

fn setup(mut cmds: Commands) {
	cmds.spawn(Camera2dBundle::default());
	cmds.spawn(ButtonBundle {
		style: Style {
			size: Size::new(Val::Px(150.0), Val::Px(65.0)),
			..default()
		},
		..default()
	})
	.with_children(|btn| {
		btn.spawn(TextBundle::from_section("Run Game", TextStyle::default()));
	});
}

pub struct RunGameEvent;
#[derive(Resource, Default, Debug)]
pub struct RunningGame(pub Option<Child>);

fn run_game(mut events: EventReader<RunGameEvent>, mut game: ResMut<RunningGame>) {
	for _event in events.iter() {
		if game.0.is_some() {
			warn!("Game is already running");
			continue;
		}
		let result = Command::new("cargo")
			.arg("run")
			.arg("--features=bevy/dynamic")
			.stdout(Stdio::piped())
			.stderr(Stdio::piped())
			// .stdin(Stdio::piped())
			.spawn();
		match result {
			Ok(mut child) => {
				let out = child.stdout.take().unwrap();
				let err = child.stderr.take().unwrap();
				game.0 = Some(child);
				let tasks = IoTaskPool::get();
				tasks
					.spawn(async move {
						let reader = BufReader::new(out);
						let mut lines = reader.lines();
						while let Some(line) = lines.next().await {
							match line {
								Ok(line) => println!("{line}"),
								Err(e) => error!("{e}"),
							}
						}
					})
					.detach();
				tasks
					.spawn(async move {
						let reader = BufReader::new(err);
						let mut lines = reader.lines();
						while let Some(line) = lines.next().await {
							match line {
								Ok(line) => eprintln!("{line}"),
								Err(e) => error!("{e}"),
							}
						}
					})
					.detach();
			}
			Err(e) => error!("{e:?}"),
		};
	}
}

fn clear_running_game(mut game: ResMut<RunningGame>) {
	if let Some(child) = game.0.as_mut() {
		match child.try_status() {
			Ok(Some(status)) => {
				info!("Game finished: {status:?}");
			}
			Ok(None) => return,
			Err(e) => {
				error!("Failed to fetch running game status: {e}")
			}
		}
	}
	game.0 = None;
}

fn run_game_btn(
	interaction_q: Query<&Interaction, (Changed<Interaction>, With<Button>)>,
	mut events: EventWriter<RunGameEvent>,
) {
	for interaction in &interaction_q {
		if *interaction == Interaction::Clicked {
			events.send(RunGameEvent)
		}
	}
}
