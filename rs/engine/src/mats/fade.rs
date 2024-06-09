use bevy::{
	pbr::{ExtendedMaterial, MaterialExtension},
	prelude::*,
	render::render_resource::{AsBindGroup, ShaderRef},
};
use serde::{Deserialize, Serialize};

#[derive(Asset, AsBindGroup, Debug, Clone, Serialize, Deserialize, Reflect)]
#[reflect(Default)]
pub struct DitherFade {
	#[uniform(200)]
	pub fade: f32,
}

impl Default for DitherFade {
	fn default() -> Self {
		Self { fade: 1.0 }
	}
}

impl DitherFade {
	pub fn new(fade: f32) -> Self {
		Self { fade }
	}
}

impl MaterialExtension for DitherFade {
	fn fragment_shader() -> ShaderRef {
		"shaders/dither_fade.wgsl".into()
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
