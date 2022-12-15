use bevy::ecs::event::Event;
use bevy::ecs::system::{EntityCommands, SystemParam, SystemParamItem};
use bevy::prelude::*;

#[inline(always)]
pub fn quantize<const BITS: u32>(value: f32) -> f32 {
	// Veltkamp-Dekker splitting algorithm
	debug_assert!(BITS < f32::MANTISSA_DIGITS);
	let d = value * (BITS + 1) as f32;
	let t = d - value;
	d - t
}

/// Like [seldom_fn_plugin](https://crates.io/crates/seldom_fn_plugin) but fns must return `&mut App`
/// just so they don't have to have a semicolon at the end
pub trait FnPluginExt {
	fn fn_plugin(&mut self, f: impl FnOnce(&mut App) -> &mut App) -> &mut Self;
}

impl FnPluginExt for App {
	fn fn_plugin(&mut self, f: impl FnOnce(&mut App) -> &mut App) -> &mut Self {
		(f)(self);
		self
	}
}

pub trait Spawnable {
	type Params: SystemParam + 'static;
	type InstanceData;
	fn spawn<'w, 's, 'a>(
		cmds: &'a mut Commands<'w, 's>,
		params: &mut SystemParamItem<'w, 's, Self::Params>,
		data: Self::InstanceData,
	) -> EntityCommands<'w, 's, 'a> where Self: Sized;
}

#[derive(SystemParam)]
pub struct Factory<'w, 's, P: Spawnable + 'static> {
	pub cmds: Commands<'w, 's>,
	pub params: SystemParamItem<'w, 's, P::Params>,
}

impl<'w, 's, T: Spawnable> Factory<'w, 's, T> {
	pub fn spawn<'a>(&'a mut self, data: T::InstanceData) -> EntityCommands<'w, 's, 'a> {
		let Self { cmds, params } = self;
		T::spawn(cmds, params, data)
	}
}

pub fn consume_spawn_events<T: Spawnable>(mut factory: Factory<T>, mut events: ResMut<Events<T::InstanceData>>)
	where <T as Spawnable>::InstanceData: Event,
{
	for event in events.drain() {
		factory.spawn(event);
	}
}
