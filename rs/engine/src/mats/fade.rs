use bevy::{
	asset::{ReflectAsset, ReflectHandle},
	pbr::{ExtendedMaterial, MaterialExtension, MaterialExtensionKey, MaterialExtensionPipeline},
	prelude::*,
	render::{
		mesh::MeshVertexBufferLayoutRef,
		render_resource::{
			AsBindGroup, RenderPipelineDescriptor, ShaderRef, SpecializedMeshPipelineError,
		},
	},
};
use serde::{Deserialize, Serialize};

#[derive(Asset, AsBindGroup, Debug, Clone, Serialize, Deserialize, Reflect)]
#[reflect(Default, Asset)]
pub struct DitherFade {
	#[uniform(200)]
	pub fade: f32,
	#[cfg(target_arch = "wasm32")]
	#[uniform(200)]
	pub _wasm_padding_8b: u32,
	#[cfg(target_arch = "wasm32")]
	#[uniform(200)]
	pub _wasm_padding_12b: u32,
	#[cfg(target_arch = "wasm32")]
	#[uniform(200)]
	pub _wasm_padding_16b: u32,
}

impl Default for DitherFade {
	fn default() -> Self {
		Self {
			fade: 1.0,
			#[cfg(target_arch = "wasm32")]
			_wasm_padding_8b: 0,
			#[cfg(target_arch = "wasm32")]
			_wasm_padding_12b: 0,
			#[cfg(target_arch = "wasm32")]
			_wasm_padding_16b: 0,
		}
	}
}

impl DitherFade {
	pub fn new(fade: f32) -> Self {
		Self {
			fade,
			#[cfg(target_arch = "wasm32")]
			_wasm_padding_8b: 0,
			#[cfg(target_arch = "wasm32")]
			_wasm_padding_12b: 0,
			#[cfg(target_arch = "wasm32")]
			_wasm_padding_16b: 0,
		}
	}
}

impl MaterialExtension for DitherFade {
	fn fragment_shader() -> ShaderRef {
		"shaders/dither_fade.wgsl".into()
	}
	fn specialize(
		pipeline: &MaterialExtensionPipeline,
		descriptor: &mut RenderPipelineDescriptor,
		layout: &MeshVertexBufferLayoutRef,
		key: MaterialExtensionKey<Self>,
	) -> Result<(), SpecializedMeshPipelineError> {
		Ok(())
	}
}

impl AsRef<DitherFade> for DitherFade {
	fn as_ref(&self) -> &DitherFade {
		self
	}
}

impl AsMut<DitherFade> for DitherFade {
	fn as_mut(&mut self) -> &mut DitherFade {
		self
	}
}

impl<B, M> AsRef<DitherFade> for ExtendedMaterial<B, M>
where
	B: Material,
	M: MaterialExtension + AsRef<DitherFade>,
{
	fn as_ref(&self) -> &DitherFade {
		self.extension.as_ref()
	}
}

impl<B, M> AsMut<DitherFade> for ExtendedMaterial<B, M>
where
	B: Material,
	M: MaterialExtension + AsMut<DitherFade>,
{
	fn as_mut(&mut self) -> &mut DitherFade {
		self.extension.as_mut()
	}
}
