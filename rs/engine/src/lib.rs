#[allow(unused_imports, clippy::single_component_path_imports)]
#[cfg(all(debug_assertions, not(target_arch = "wasm32")))]
use bevy_dylib;

pub mod anim;
pub mod mats;
pub mod nav;
pub mod offloading;
pub mod planet;
pub mod settings;
pub mod util;
