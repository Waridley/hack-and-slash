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

pub type TaskHandle<T = ()> = <TaskOffloader<'static, 'static> as Offload>::Task<T>;

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
	crate::bevy_test!(fn spawn_many_tasks(app, test_id) {
		use bevy::prelude::*;
		use crate::{
			offloading::{Offload, OffloadedTask, TaskOffloader, TaskHandle},
			testing::{TestSystem, TestStatus},
		};

		fn spawn_tasks(
			mut offloader: TaskOffloader,
			mut spawned: Local<bool>,
			mut running: Local<Vec<TaskHandle>>,
		) -> TestStatus {
			if !*spawned {
				trace!("Hello from async task!");
				for task in 0..10_000 {
					running.push(offloader
						.start(async move {
							trace!("Starting task {task}...");
							for i in task..task + 1_000_000u64 {
								std::hint::black_box(i);
								if i % 10_000 == 0 {
									trace!("Task {task} yielding at {i}");
									futures_lite::future::yield_now().await;
								}
							}
							trace!("Task {task} done!");
						}));
				}
				*spawned = true;
			}
			running.retain_mut(|handle| handle.check().is_none());
			if running.is_empty() {
				TestStatus::Passed
			} else {
				TestStatus::Running
			}
		}

		app.add_systems(Update, spawn_tasks.test(test_id));
	});
}

#[inline(always)]
pub async fn wasm_yield() {
	#[cfg(target_arch = "wasm32")]
	::futures_lite::future::yield_now().await
}
