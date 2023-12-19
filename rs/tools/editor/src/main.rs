use async_process::{Child, ChildStderr, ChildStdout, Command, Stdio};
use bevy::{
	ecs::system::SystemId, prelude::*, tasks::IoTaskPool,
};
use futures_lite::{io::BufReader, AsyncBufReadExt, StreamExt};

fn main() {
	let mut app = App::new();
	app.add_plugins(DefaultPlugins)
		.init_resource::<RunningGame>()
		.add_systems(Startup, setup)
		.add_systems(First, clear_running_game)
		.add_systems(Update, run_game_btn);
	let run_game_system = app.world.register_system(run_game_as_child_process);
	app.insert_resource(RunGameSystem(run_game_system));
	app.run()
}

fn setup(mut cmds: Commands) {
	cmds.spawn(Camera2dBundle::default());
	cmds.spawn(ButtonBundle {
		style: Style {
			width: Val::Px(150.0),
			height: Val::Px(65.0),
			..default()
		},
		..default()
	})
	.with_children(|btn| {
		btn.spawn(TextBundle::from_section("Run Game", TextStyle::default()));
	});
}

#[derive(Resource)]
struct RunGameSystem(SystemId);
#[derive(Resource, Default, Debug)]
struct RunningGame(pub Option<Child>);

#[allow(dead_code)]
fn run_game_as_child_process(mut game: ResMut<RunningGame>) {
	info!("Starting game...");
	if game.0.is_some() {
		warn!("Game is already running");
		return;
	}
	let result = Command::new("cargo")
		.arg("run")
		.arg("--features=bevy/file_watcher,bevy/asset_processor,bevy/bevy_dynamic_plugin,debugging")
		.arg("--profile=desktop")
		.arg("--color=always")
		.stdout(Stdio::piped())
		.stderr(Stdio::piped())
		// .stdin(Stdio::piped())
		.spawn();
	match result {
		Ok(mut child) => {
			let out = child.stdout.take().unwrap();
			let err = child.stderr.take().unwrap();
			game.0 = Some(child);
			pipe_child_outputs(out, err);
		}
		Err(e) => error!("{e:?}"),
	};
}

fn clear_running_game(mut game: ResMut<RunningGame>) {
	if let Some(child) = game.0.as_mut() {
		match child.try_status() {
			Ok(Some(status)) => info!("Game finished: {status:?}"),
			Ok(None) => return,
			Err(e) => {
				error!("Failed to fetch running game status: {e}");
				match child.kill() {
					Ok(()) => info!("Killed child process to avoid indefinite running."),
					Err(e) => {
						error!("Failed to kill child process, it might still be running: {e}")
					}
				}
			}
		}
	}
	game.0 = None
}

fn pipe_child_outputs(out: ChildStdout, err: ChildStderr) {
	let pool = IoTaskPool::get();
	pool.spawn(async move {
		let reader = BufReader::new(out);
		let mut lines = reader.lines();
		while let Some(line) = lines.next().await {
			match line {
				Ok(line) => println!(">\t{line}"),
				Err(e) => error!("{e}"),
			}
		}
	})
	.detach();
	pool.spawn(async move {
		let reader = BufReader::new(err);
		let mut lines = reader.lines();
		while let Some(line) = lines.next().await {
			match line {
				Ok(line) => eprintln!(">\t{line}"),
				Err(e) => error!("{e}"),
			}
		}
	})
	.detach();
}

fn run_game_btn(
	mut cmds: Commands,
	interaction_q: Query<&Interaction, (Changed<Interaction>, With<Button>)>,
	run_game_system: Res<RunGameSystem>,
) {
	for interaction in &interaction_q {
		if *interaction == Interaction::Pressed {
			info!("Sending run game command");
			cmds.run_system(run_game_system.0)
		}
	}
}
