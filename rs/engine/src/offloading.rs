use bevy::prelude::*;
use std::{any::Any, future::Future};

#[cfg(target_arch = "wasm32")]
mod wasm;
#[cfg(target_arch = "wasm32")]
pub use wasm::*;

#[cfg(not(target_arch = "wasm32"))]
mod desktop;
#[cfg(not(target_arch = "wasm32"))]
pub use desktop::*;

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

// TODO: Make these private again, by separating game test and engine test concerns.
#[cfg(feature = "testing")]
pub mod tests {
	use crate::offloading::{Offload, OffloadedTask, TaskOffloader};
	use bevy::prelude::*;
	use std::sync::atomic::{AtomicBool, Ordering::Relaxed};

	pub fn spawning(mut offloader: TaskOffloader, t: Res<Time>) {
		static SPAWNED: AtomicBool = AtomicBool::new(false);
		if !SPAWNED.load(Relaxed) && t.elapsed_seconds() > 3.0 {
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
			bevy::log::info!("Done spawning counting tasks");
			SPAWNED.store(true, Relaxed);
		}
	}
}

#[inline(always)]
pub async fn wasm_yield() {
	#[cfg(target_arch = "wasm32")]
	::futures_lite::future::yield_now().await
}
