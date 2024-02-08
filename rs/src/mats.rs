use bevy::{
	pbr::MaterialExtension,
	prelude::*,
	reflect::TypeUuid,
	render::render_resource::{AsBindGroup, ShaderRef},
};
use serde::{Deserialize, Serialize};

#[derive(Asset, AsBindGroup, Debug, Clone, TypeUuid, Serialize, Deserialize, Reflect)]
#[reflect(Default)]
#[uuid = "32c44b20-ae2c-43b2-96c8-aa9bb12d6a8b"]
pub struct BubbleMaterial {
	#[uniform(100)]
	pub glow_color: Color,
}

impl Default for BubbleMaterial {
	fn default() -> Self {
		Self {
			glow_color: Color::GRAY,
		}
	}
}

impl MaterialExtension for BubbleMaterial {
	fn fragment_shader() -> ShaderRef {
		"shaders/bubble.wgsl".into()
	}

	// fn vertex_shader() -> ShaderRef {
	// 	"shaders/bubble.wgsl".into()
	// }
}
