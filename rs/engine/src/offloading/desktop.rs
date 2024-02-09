use super::{Offload, OffloadedTask};
use bevy::{ecs::system::SystemParam, prelude::*, tasks::AsyncComputeTaskPool};
use std::{
	any::Any,
	future::Future,
	marker::PhantomData,
	pin::Pin,
	task::{Context, Poll},
};

#[derive(SystemParam, Debug)]
pub struct TaskOffloader<'w, 's>(PhantomData<Commands<'w, 's>>);

mod private {
	use std::any::Any;

	// Don't want to accidentally use functions from bevy's task on desktop and then not have them
	// available on WASM.
	pub struct Task<Out: Any + Send + 'static>(pub(super) bevy::tasks::Task<Out>);
}

impl Offload for TaskOffloader<'_, '_> {
	type Task<Out: Any + Send + 'static> = private::Task<Out>;

	fn start<Out: Send + Sync + 'static>(
		&mut self,
		task: impl Future<Output = Out> + Send + Sync + 'static,
	) -> Self::Task<Out> {
		private::Task(AsyncComputeTaskPool::get().spawn(task))
	}
}

impl<Out: Any + Send + 'static> OffloadedTask<Out> for private::Task<Out> {
	fn let_go(self) {
		self.0.detach()
	}
}

impl<Out: Any + Send + 'static> Future for private::Task<Out> {
	type Output = Out;

	fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
		unsafe { self.map_unchecked_mut(|this| &mut this.0) }.poll(cx)
	}
}
