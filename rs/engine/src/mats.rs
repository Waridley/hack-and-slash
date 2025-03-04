use bevy::{
	pbr::{ExtendedMaterial, MaterialExtension},
	prelude::*,
	render::render_resource::{AsBindGroup, ShaderRef},
};
use serde::{Deserialize, Serialize};

use crate::{
	mats::{
		fade::DitherFade,
		fog::{update_matter_extensions, Matter, MatterPlugin},
	},
	util::RonReflectAssetLoader,
};

pub mod fade;
pub mod fog;

/// Extend the [Matter] material. Most materials in the game should
/// extend this in order to have fog applied.
pub type ExtMat<M> = ExtendedMaterial<Matter, M>;
/// Shorthand for `ExtendedMaterial<StandardMaterial<T>`.
/// Use this when [DistanceDither] should not be applied.
pub type StdMatExt<T> = ExtendedMaterial<StandardMaterial, T>;

pub struct MatsPlugin;

impl Plugin for MatsPlugin {
	fn build(&self, app: &mut App) {
		Box::leak(Box::new(
			app.world()
				.resource::<AssetServer>()
				.load::<Shader>("shaders/util.wgsl"),
		));

		app.add_systems(Update, (update_matter_extensions::<BubbleMaterial>,))
			.add_plugins((
				MatterPlugin,
				MaterialPlugin::<ExtMat<BubbleMaterial>>::default(),
				MaterialPlugin::<ExtMat<DitherFade>>::default(),
			));

		let registry = app
			.world()
			.get_resource::<AppTypeRegistry>()
			.unwrap()
			.clone();
		{
			let mut reg = registry.write();
			reg.register::<BubbleMaterial>();
			reg.register::<ExtMat<BubbleMaterial>>();
		}
		app.register_asset_loader(RonReflectAssetLoader::<ExtMat<BubbleMaterial>>::new(
			registry.clone(),
			vec!["mat.ron"],
		));
	}
}

#[derive(Asset, AsBindGroup, Debug, Clone, Serialize, Deserialize, Reflect)]
#[reflect(Default)]
pub struct BubbleMaterial {
	#[uniform(200)]
	pub glow_color: LinearRgba,
}

impl Default for BubbleMaterial {
	fn default() -> Self {
		Self {
			glow_color: LinearRgba::WHITE,
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
