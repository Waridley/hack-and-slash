use bevy::prelude::*;

#[inline(always)]
pub const fn truncate_mantissa<const BITS: u32>(value: f32) -> f32 {
	debug_assert!(BITS < f32::MANTISSA_DIGITS);
	let mask = u32::MAX << (f32::MANTISSA_DIGITS - BITS);
	unsafe {
		// f32::to_bits and f32::from_bits are const unstable
		// check their source code for why this is safe on all platforms we care about
		std::mem::transmute(std::mem::transmute::<_, u32>(value) & mask)
	}
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
