use super::icons::{Icon, IconBundleBuilder};
use crate::ui::{
	widgets::{Font3d, WidgetShape},
	TextMeshCache, GLOBAL_UI_RENDER_LAYERS,
};
use bevy::{ecs::system::EntityCommands, prelude::*, render::view::RenderLayers};
use bevy_rapier3d::parry::shape::SharedShape;
use bevy_svg::prelude::Origin;

#[derive(Clone, Debug)]
pub struct IconWidgetBuilder {
	pub icon: Icon,
	pub font: Handle<Font3d>,
	pub origin: Origin,
	pub size: Vec3,
	pub transform: Transform,
	pub global_transform: GlobalTransform,
	pub visibility: Visibility,
	pub inherited_visibility: InheritedVisibility,
	pub layers: RenderLayers,
}

impl Default for IconWidgetBuilder {
	fn default() -> Self {
		Self {
			icon: default(),
			font: default(),
			origin: default(),
			size: Vec3::ONE,
			transform: default(),
			global_transform: default(),
			visibility: default(),
			inherited_visibility: default(),
			layers: GLOBAL_UI_RENDER_LAYERS,
		}
	}
}

impl IconWidgetBuilder {
	pub fn build(
		self,
		asset_server: &AssetServer,
		meshes: Mut<Assets<Mesh>>,
		cache: Mut<TextMeshCache>,
		fonts: Mut<Assets<Font3d>>,
	) -> (impl Bundle, Option<impl Bundle>) {
		let Self {
			icon,
			font,
			origin,
			size,
			transform,
			global_transform,
			visibility,
			inherited_visibility,
			layers,
		} = self;
		let widget = WidgetShape(SharedShape::cuboid(
			size.x * 0.5,
			size.y * 0.5,
			size.z * 0.5,
		));
		let (svg, text) = IconBundleBuilder {
			icon,
			font,
			origin,
			size,
			transform,
			global_transform,
			visibility,
			inherited_visibility,
			layers,
		}
		.build(asset_server, meshes, cache, fonts);
		((widget, svg), text)
	}

	pub fn spawn<'a>(
		self,
		cmds: &'a mut Commands,
		asset_server: &AssetServer,
		meshes: Mut<Assets<Mesh>>,
		cache: Mut<TextMeshCache>,
		fonts: Mut<Assets<Font3d>>,
	) -> EntityCommands<'a> {
		let (image, text) = self.build(asset_server, meshes, cache, fonts);
		let mut cmds = cmds.spawn(image);
		text.map(|text| {
			cmds.with_children(|cmds| {
				cmds.spawn(text);
			})
		});
		cmds
	}
}
