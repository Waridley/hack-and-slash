use bevy::{
	asset::Asset,
	pbr::{ExtendedMaterial, MaterialExtension},
	prelude::*,
	render::{
		extract_resource::ExtractResourcePlugin,
		render_asset::RenderAssetUsages,
		render_resource::{AsBindGroup, ShaderRef},
		texture::{
			ImageAddressMode, ImageFilterMode, ImageSampler, ImageSamplerDescriptor, ImageType,
		},
	},
};
use serde::{Deserialize, Serialize};

use crate::{
	planet::{chunks::CHUNK_SCALE, weather::Weather},
	util::RonReflectAssetLoader,
};

pub type ExtMat<M> = ExtendedMaterial<Matter, M>;
pub type Matter = ExtendedMaterial<StandardMaterial, DistanceDither>;

/// Function instead of a constant because it uses floating-point math.
#[inline(always)]
pub fn max_fog_distance() -> f32 {
	(CHUNK_SCALE.x.powi(2) + CHUNK_SCALE.y.powi(2)).sqrt() * 1.5
}

pub struct MatterPlugin;

impl Plugin for MatterPlugin {
	fn build(&self, app: &mut App) {
		app.init_resource::<Weather>()
			.add_plugins((
				MaterialPlugin::<Matter>::default(),
				ExtractResourcePlugin::<Weather>::default(),
			))
			.add_systems(Update, (update_matter_globals,));

		let registry = app.world.get_resource::<AppTypeRegistry>().unwrap().clone();
		{
			let mut reg = registry.write();
			reg.register::<DistanceDither>();
			reg.register::<Matter>();
		}
		app.register_asset_loader(RonReflectAssetLoader::<Matter>::new(
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
			RenderAssetUsages::RENDER_WORLD,
		)
		.unwrap();

		assets.insert(BAYER_HANDLE, image);
	}
}

#[derive(Asset, AsBindGroup, Debug, Clone, Serialize, Deserialize, Reflect)]
#[reflect(Default)]
pub struct DistanceDither {
	/// Distance from the camera at which the mesh starts to fade into the background.
	#[uniform(100)]
	pub far_start: f32,
	/// Distance from the camera at which the mesh is fully discarded.
	#[uniform(100)]
	pub far_end: f32,

	/// Distance from the camera at which the mesh starts to fade away.
	#[uniform(100)]
	pub near_start: f32,
	/// Distance from the camera at which the mesh is fully discarded.
	#[uniform(100)]
	pub near_end: f32,

	#[serde(skip)]
	#[texture(101)]
	#[sampler(102)]
	pub matrix: Handle<Image>,
}

impl DistanceDither {
	pub fn new(start: f32, end: f32, matrix: Handle<Image>) -> Self {
		Self {
			far_start: start,
			far_end: end,
			near_start: 32.0,
			near_end: 0.0,
			matrix,
		}
	}
}

pub const BAYER_HANDLE: Handle<Image> =
	Handle::weak_from_u128(92299220200241619468604683494190943784);

impl Default for DistanceDither {
	fn default() -> Self {
		Self::new(
			max_fog_distance() * 0.5,
			max_fog_distance(),
			BAYER_HANDLE.clone(),
		)
	}
}

impl MaterialExtension for DistanceDither {
	fn fragment_shader() -> ShaderRef {
		"shaders/dist_dither.wgsl".into()
	}
}

pub trait IntoMatter {
	fn into_matter(self) -> Matter;
}

impl<T: Into<StandardMaterial>> IntoMatter for T {
	fn into_matter(self) -> Matter {
		Matter {
			base: self.into(),
			extension: default(),
		}
	}
}

// FIXME: This is a very lazy way to change the global fog parameters.
// Should really specialize the render pipeline, maybe even replacing the
// built-in skybox pipeline.
pub fn update_matter_globals(mut materials: ResMut<Assets<Matter>>, fog: Res<Weather>) {
	if !fog.is_changed() {
		return;
	}

	for (_, mat) in materials.iter_mut() {
		mat.extension.far_start = fog.fog_start;
		mat.extension.far_end = fog.fog_end;
	}
}

pub fn update_matter_extensions<M: MaterialExtension>(
	mut materials: ResMut<Assets<ExtMat<M>>>,
	fog: Res<Weather>,
) {
	if !fog.is_changed() {
		return;
	}

	for (_, mat) in materials.iter_mut() {
		mat.base.extension.far_start = fog.fog_start;
		mat.base.extension.far_end = fog.fog_end;
	}
}
