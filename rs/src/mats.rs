use bevy::{
	pbr::MaterialExtension,
	prelude::*,
	reflect::TypeUuid,
	render::render_resource::{AsBindGroup, ShaderRef},
};
use serde::{Deserialize, Serialize};

#[derive(Asset, AsBindGroup, Debug, Clone, TypeUuid, Serialize, Deserialize, Reflect)]
#[uuid = "32c44b20-ae2c-43b2-96c8-aa9bb12d6a8b"]
pub struct BubbleMaterial {
	#[uniform(100)]
	pub color: Color,
	#[uniform(100)]
	pub intensity: f32,
}

impl Default for BubbleMaterial {
	fn default() -> Self {
		Self {
			color: Color::GRAY,
			intensity: 1.0,
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
