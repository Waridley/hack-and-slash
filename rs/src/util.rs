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
