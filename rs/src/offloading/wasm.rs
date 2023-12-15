use std::any::Any;
use std::cell::Cell;
use std::future::Future;
use std::pin::Pin;
use std::ptr::NonNull;
use std::sync::{Arc, Weak};
use std::task::{Context, Poll};
use bevy::ecs::system::SystemParam;
use bevy::prelude::*;
use bevy::tasks::AsyncComputeTaskPool;
use bevy::utils::HashMap;
use futures_lite::FutureExt;
use rapier3d::crossbeam::atomic::AtomicCell;
use crate::offloading::{Offload, OffloadedTask};

#[derive(SystemParam)]
pub struct TaskOffloader<'w, 's> {
	cmds: Commands<'w, 's>,
}

type OutputSlot = AtomicCell<Poll<DynOutput>>;
type DynOutput = Box<dyn Any + Send + 'static>;
type DynTask = Box<dyn Future<Output = DynOutput> + Send + Sync + 'static>;

mod private {
	use std::{future::Future, sync::Arc, any::Any, task::Poll};
	use std::marker::PhantomData;
	use std::sync::Weak;
	use bevy::prelude::{Component, Entity};
	use rapier3d::crossbeam::atomic::AtomicCell;
	use super::{OutputSlot, DynTask};
	pub struct Task<Out: Any + Send + 'static>(pub(super) Entity, pub(super) Arc<OutputSlot>, pub(super) PhantomData<Out>);
	#[derive(Component)]
	pub struct TaskComponent {
		pub(super) status: TaskStatus,
	}
	
	pub enum TaskStatus {
		Pending {
			f: DynTask,
			out: Weak<OutputSlot>,
		},
		Running,
		Finished(Weak<OutputSlot>)
	}
}

impl Offload for TaskOffloader<'_, '_> {
	type Task<Out: Any + Send + 'static> = private::Task<Out>;
	
	fn start<Out: Any + Send>(&mut self, task: impl Future<Output = Out> + Send + Sync + 'static) -> Self::Task<Out> {
		let out = Arc::new(AtomicCell::new(Poll::Pending));
		private::Task(
			self.cmds.spawn(private::TaskComponent {
				status: private::TaskStatus::Pending {
					f: Box::new(async move { Box::new(task.await) as Box<dyn Any + Send + 'static> }) as _,
					out: Arc::downgrade(&out),
				}
			}).id(),
			out,
			default(),
		)
	}
}

impl<Out: Any + Send + 'static> Future for private::Task<Out> {
	type Output = Out;
	
	fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
		match self.1.swap(Poll::Pending) {
			Poll::Ready(out) => {
				let out = *out.downcast::<Out>().expect("The only way to get a `Task<Out>` should be via `TaskOffloader::start::<Out>`");
				Poll::Ready(out)
			},
			Poll::Pending => Poll::Pending,
		}
	}
}

impl<Out: Any + Send + 'static> OffloadedTask<Out> for private::Task<Out> {
	fn let_go(self) {
		// Drop Arc, causing Weak to fail to upgrade and entity to despawn when finished
	}
}

/// Returns Ready on every poll to split tasks over multiple frames
struct Microtask<'a>(&'a mut (dyn Future<Output = DynOutput> + Send + Sync));

impl Future for Microtask<'_> {
	type Output = Poll<Box<dyn Any + Send + 'static>>;
	
	fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
		Poll::Ready(unsafe { self.map_unchecked_mut(|it| it.0) }.poll(cx))
	}
}

pub fn tick_offloaded_tasks(
	mut cmds: Commands,
	mut q: Query<(Entity, &mut private::TaskComponent)>,
) {
	use private::TaskStatus::*;
	for (id, mut task) in &mut q {
		match std::mem::replace(&mut task.status, Running) {
			Pending { mut f, out, } => {
				let result = futures_lite::future::block_on(async move {
					let result = Microtask(&mut *f).await;
					match result {
						Poll::Pending => {
							out.upgrade().map(|out| out.store(Poll::Pending));
							Pending { f, out }
						},
						Poll::Ready(result) => {
							out.upgrade().map(|out| out.store(Poll::Ready(result)));
							Finished(out)
						}
					}
				});
				task.status = result;
			},
			Running => continue,
			Finished(out) => {
				if out.upgrade().is_none() {
					cmds.entity(id).despawn();
				} else {
					task.status = Finished(out);
				}
			},
		}
	}
}
