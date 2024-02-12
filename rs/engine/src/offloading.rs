use std::{any::Any, future::Future};

use bevy::prelude::*;

#[cfg(not(target_arch = "wasm32"))]
pub use desktop::*;
#[cfg(target_arch = "wasm32")]
pub use wasm::*;

#[cfg(not(target_arch = "wasm32"))]
mod desktop;
#[cfg(target_arch = "wasm32")]
mod wasm;

pub struct OffloadingPlugin;

impl Plugin for OffloadingPlugin {
	fn build(&self, _app: &mut App) {
		#[cfg(target_arch = "wasm32")]
		_app.add_systems(Update, wasm::tick_offloaded_tasks);
	}
}

pub type TaskHandle<T> = <TaskOffloader<'static, 'static> as Offload>::Task<T>;

pub trait Offload {
	type Task<Out: Any + Send + 'static>: OffloadedTask<Out>;

	fn start<Out: Send + Sync + 'static>(
		&mut self,
		task: impl Future<Output = Out> + Send + Sync + 'static,
	) -> Self::Task<Out>;
}

pub trait OffloadedTask<Out: 'static>: Future<Output = Out> {
	fn let_go(self);
	fn check(&mut self) -> Option<Out>
	where
		Self: Unpin,
	{
		futures_lite::future::block_on(futures_lite::future::poll_once(self))
	}
}

#[cfg(feature = "testing")]
pub(crate) mod tests {
	use bevy::prelude::*;

	use crate::{
		offloading::{Offload, OffloadedTask, TaskOffloader},
		testing::*,
	};

	#[linkme::distributed_slice(TESTS)]
	pub fn _spawn_many_tasks(app: &mut App) -> &'static str {
		pub fn spawn_tasks(
			mut offloader: TaskOffloader,
			t: Res<Time>,
			mut spawned: Local<bool>,
			mut events: EventWriter<TestEvent>,
		) {
			if !*spawned && t.elapsed_seconds() > 3.0 {
				bevy::log::info!("Hello from async task!");
				for task in 0..100 {
					offloader
						.start(async move {
							bevy::log::info!("Starting task {task}...");
							for i in task..task + 1_000_000u64 {
								std::hint::black_box(i);
								if i % 10_000 == 0 {
									bevy::log::info!("Task {task} yielding at {i}");
									futures_lite::future::yield_now().await;
								}
							}
							bevy::log::info!("Task {task} done!");
						})
						.let_go();
				}
				events.send(TestEvent {
					name: "spawn_many_tasks",
					status: TestStatus::Passed,
				});
				*spawned = true;
			}
		}
		app.add_systems(Update, spawn_tasks);
		"spawn_many_tasks"
	}
	#[cfg_attr(not(feature = "render"), test)]
	fn spawn_many_tasks() {
		let mut app = app();
		_spawn_many_tasks(&mut app);
		app.run();
	}
}

#[inline(always)]
pub async fn wasm_yield() {
	#[cfg(target_arch = "wasm32")]
	::futures_lite::future::yield_now().await
}
