use super::icons::{Icon, IconBundleBuilder};
use crate::ui::{
	widgets::{Font3d, WidgetShape, UNLIT_MATERIAL_ID},
	TextMeshCache, GLOBAL_UI_RENDER_LAYERS,
};
use bevy::{ecs::system::EntityCommands, prelude::*, render::view::RenderLayers};
use bevy_rapier3d::parry::shape::SharedShape;

#[derive(Clone, Debug)]
pub struct IconWidgetBuilder<M: Material = StandardMaterial> {
	pub icon: Icon,
	pub font: Handle<Font3d>,
	pub size: Vec3,
	pub transform: Transform,
	pub global_transform: GlobalTransform,
	pub visibility: Visibility,
	pub inherited_visibility: InheritedVisibility,
	pub layers: RenderLayers,
	pub material: Handle<M>,
}

impl<M: Material> Default for IconWidgetBuilder<M> {
	fn default() -> Self {
		Self {
			icon: default(),
			font: default(),
			size: Vec3::ONE,
			transform: default(),
			global_transform: default(),
			visibility: default(),
			inherited_visibility: default(),
			layers: GLOBAL_UI_RENDER_LAYERS,
			material: Handle::weak_from_u128(UNLIT_MATERIAL_ID),
		}
	}
}

impl<M: Material> IconWidgetBuilder<M> {
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
			size,
			transform,
			global_transform,
			visibility,
			inherited_visibility,
			layers,
			material,
		} = self;
		// TODO: Should be `Aabb`, but that doesn't implement `Shape`
		let widget = WidgetShape(SharedShape::cuboid(
			size.x * 0.5,
			size.y * 0.5,
			size.z * 0.5,
		));
		let (svg, text) = IconBundleBuilder {
			icon,
			font,
			size,
			transform,
			global_transform,
			visibility,
			inherited_visibility,
			layers,
			material,
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
