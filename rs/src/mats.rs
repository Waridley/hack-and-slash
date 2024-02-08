use crate::{
	planet::chunks::{CHUNK_COLS, CHUNK_SCALE, TERRAIN_CELL_SIZE},
	util::RonReflectAssetLoader,
};
use bevy::{
	asset::load_internal_binary_asset,
	pbr::{ExtendedMaterial, MaterialExtension},
	prelude::*,
	reflect::TypeUuid,
	render::{
		render_resource::{AsBindGroup, ShaderRef},
		texture::{
			ImageAddressMode, ImageFilterMode, ImageSampler, ImageSamplerDescriptor, ImageType,
		},
	},
};
use serde::{Deserialize, Serialize};

pub type StdMatExt<T> = ExtendedMaterial<StandardMaterial, T>;

pub struct MatsPlugin;

impl Plugin for MatsPlugin {
	fn build(&self, app: &mut App) {
		app.add_plugins((
			MaterialPlugin::<StdMatExt<BubbleMaterial>>::default(),
			MaterialPlugin::<StdMatExt<DistanceDither>>::default(),
		));

		let registry = app.world.get_resource::<AppTypeRegistry>().unwrap().clone();
		{
			let mut reg = registry.write();
			reg.register::<BubbleMaterial>();
			reg.register::<StdMatExt<BubbleMaterial>>();
			reg.register::<DistanceDither>();
			reg.register::<StdMatExt<DistanceDither>>();
		}
		app.register_asset_loader(RonReflectAssetLoader::<StdMatExt<BubbleMaterial>>::new(
			registry.clone(),
			vec!["mat.ron"],
		))
		.register_asset_loader(RonReflectAssetLoader::<StdMatExt<DistanceDither>>::new(
			registry,
			vec!["mat.ron"],
		));
	}

	fn finish(&self, app: &mut App) {
		let mut assets = app.world.resource_mut::<Assets<Image>>();

		let image = Image::from_buffer(
			include_bytes!("bayer16.png").as_ref(),
			ImageType::Extension("png"),
			default(),
			false,
			ImageSampler::Descriptor(ImageSamplerDescriptor {
				label: None,
				address_mode_u: ImageAddressMode::Repeat,
				address_mode_v: ImageAddressMode::Repeat,
				address_mode_w: ImageAddressMode::ClampToEdge,
				mag_filter: ImageFilterMode::Nearest,
				min_filter: ImageFilterMode::Nearest,
				mipmap_filter: ImageFilterMode::Nearest,
				lod_min_clamp: 0.0,
				lod_max_clamp: 0.0,
				compare: None,
				anisotropy_clamp: 1,
				border_color: None,
			}),
		)
		.unwrap();

		assets.insert(BAYER_HANDLE, image);
	}
}

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

#[derive(Asset, AsBindGroup, Debug, Clone, TypeUuid, Serialize, Deserialize, Reflect)]
#[reflect(Default)]
#[uuid = "ff77386c-1d31-4afd-b52f-481b0845de0d"]
pub struct DistanceDither {
	#[uniform(100)]
	pub start: f32,
	#[uniform(100)]
	pub end: f32,

	#[serde(skip)]
	#[texture(101)]
	#[sampler(102)]
	pub matrix: Handle<Image>,
}

impl DistanceDither {
	pub fn new(start: f32, end: f32, matrix: Handle<Image>) -> Self {
		Self { start, end, matrix }
	}
}

pub const BAYER_HANDLE: Handle<Image> =
	Handle::weak_from_u128(92299220200241619468604683494190943784);

impl Default for DistanceDither {
	fn default() -> Self {
		Self::new(CHUNK_SCALE.x, CHUNK_SCALE.x * 2.0, BAYER_HANDLE.clone())
	}
}

impl MaterialExtension for DistanceDither {
	fn fragment_shader() -> ShaderRef {
		"shaders/dist_dither.wgsl".into()
	}
}
