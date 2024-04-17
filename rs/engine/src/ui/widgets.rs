use super::in_map::icons::{Icon, IconBundleBuilder};
use crate::ui::TextMeshCache;
use crate::ui::GLOBAL_UI_RENDER_LAYERS;
use bevy::{
	ecs::system::EntityCommands,
	prelude::*,
	render::{
		mesh::PrimitiveTopology::TriangleList, render_asset::RenderAssetUsages, view::RenderLayers,
	},
	utils::CowArc,
};
use bevy_svg::prelude::Origin;
use meshtext::{MeshGenerator, MeshText, OwnedFace, TextSection};
use rapier3d::parry::shape::SharedShape;
use std::fmt::{Debug, Formatter};

#[derive(Asset, Deref, DerefMut, TypePath)]
pub struct Font3d(pub MeshGenerator<OwnedFace>);

#[derive(Component, Clone, Deref, DerefMut)]
pub struct WidgetShape(pub SharedShape);

impl Default for WidgetShape {
	fn default() -> Self {
		Self(SharedShape::cuboid(0.5, 0.5, 0.5))
	}
}

impl Debug for WidgetShape {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		ron::to_string(&self.0).fmt(f)
	}
}

#[derive(Bundle, Clone, Debug)]
pub struct WidgetBundle {
	pub shape: WidgetShape,
	pub transform: Transform,
	pub global_transform: GlobalTransform,
	pub visibility: Visibility,
	pub inherited_visibility: InheritedVisibility,
	pub view_visibility: ViewVisibility,
	pub layers: RenderLayers,
}

impl Default for WidgetBundle {
	fn default() -> Self {
		Self {
			shape: default(),
			transform: default(),
			global_transform: default(),
			visibility: default(),
			inherited_visibility: default(),
			view_visibility: default(),
			layers: GLOBAL_UI_RENDER_LAYERS,
		}
	}
}

#[derive(Clone, Debug)]
pub struct PanelBuilder<M: Material> {
	pub size: Vec3,
	pub material: Handle<M>,
	pub transform: Transform,
	pub global_transform: GlobalTransform,
	pub visibility: Visibility,
	pub inherited_visibility: InheritedVisibility,
	pub layers: RenderLayers,
}

impl<M: Material> Default for PanelBuilder<M> {
	fn default() -> Self {
		Self {
			size: Vec3::ONE,
			material: default(),
			transform: default(),
			global_transform: default(),
			visibility: default(),
			inherited_visibility: default(),
			layers: GLOBAL_UI_RENDER_LAYERS,
		}
	}
}

impl<M: Material> PanelBuilder<M> {
	pub fn build(self, meshes: &mut Assets<Mesh>) -> impl Bundle {
		let Self {
			size,
			material,
			transform,
			global_transform,
			visibility,
			inherited_visibility,
			layers,
		} = self;
		let mesh = meshes.add(
			Cuboid::new(size.x, size.y, size.z)
				.mesh()
				.with_duplicated_vertices()
				.with_computed_flat_normals(),
		);
		(
			WidgetBundle {
				shape: WidgetShape(SharedShape::cuboid(
					size.x * 0.5,
					size.y * 0.5,
					size.z * 0.5,
				)),
				transform,
				global_transform,
				visibility,
				inherited_visibility,
				view_visibility: default(),
				layers,
			},
			mesh,
			material,
		)
	}
}

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

pub const DEFAULT_TEXT_MAT: Handle<StandardMaterial> = Handle::Weak(AssetId::Uuid {
	uuid: AssetId::<StandardMaterial>::DEFAULT_UUID,
});

#[derive(Clone, Debug)]
pub struct TextBuilder<M: Material = StandardMaterial> {
	pub text: CowArc<'static, str>,
	pub font: Handle<Font3d>,
	pub flat: bool,
	pub material: Handle<M>,
	pub vertex_translation: Vec3,
	pub vertex_rotation: Quat,
	pub vertex_scale: Vec3,
	pub transform: Transform,
	pub global_transform: GlobalTransform,
	pub visibility: Visibility,
	pub layers: RenderLayers,
}

impl Default for TextBuilder {
	fn default() -> Self {
		Self {
			text: default(),
			font: default(),
			flat: true,
			material: DEFAULT_TEXT_MAT.clone(),
			vertex_translation: default(),
			vertex_rotation: Quat::from_rotation_arc(Vec3::Z, Vec3::NEG_Y),
			vertex_scale: Vec3::new(1.0, 1.0, 0.4),
			transform: default(),
			global_transform: default(),
			visibility: default(),
			layers: GLOBAL_UI_RENDER_LAYERS,
		}
	}
}

impl<M: Material> TextBuilder<M> {
	pub fn build(
		self,
		mut meshes: Mut<Assets<Mesh>>,
		mut cache: Mut<TextMeshCache>,
		fonts: Mut<Assets<Font3d>>,
	) -> Option<impl Bundle> {
		let Self {
			text,
			flat,
			font,
			material,
			vertex_translation,
			vertex_rotation,
			vertex_scale,
			transform,
			global_transform,
			visibility,
			layers,
		} = self;

		let (mesh, shape) = cache
			.entry(text.clone())
			.or_insert_with(|| {
				let mut font = fonts.map_unchanged(|fonts| fonts.get_mut(font).unwrap());
				let MeshText { bbox, vertices } = font
					.generate_section(
						&text,
						flat,
						Some(
							&Mat4::from_scale_rotation_translation(
								vertex_scale,
								vertex_rotation,
								vertex_translation,
							)
							.to_cols_array(),
						),
					)
					.map_err(|e|
						// `:?` because `GlyphTriangulationError` has a useless `Display` impl
						error!("{e:?}"))
					.ok()?;

				let shape = WidgetShape(SharedShape::cuboid(
					bbox.size().x * 0.5,
					bbox.size().y * 0.5,
					bbox.size().z * 0.5,
				));

				let verts = vertices
					.chunks(3)
					.map(|c| [c[0], c[1], c[2]])
					.collect::<Vec<_>>();
				let len = verts.len();
				let mut mesh = Mesh::new(TriangleList, RenderAssetUsages::RENDER_WORLD);
				mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, verts);
				mesh.insert_attribute(Mesh::ATTRIBUTE_UV_0, vec![[0.0, 0.0]; len]);
				mesh.compute_flat_normals();
				Some((meshes.add(mesh), shape))
			})
			.clone()?;

		Some((
			MaterialMeshBundle {
				mesh,
				material,
				transform,
				global_transform,
				visibility,
				inherited_visibility: default(),
				..default()
			},
			shape,
			layers,
		))
	}
}

#[derive(Component, Default, Debug, Copy, Clone)]
pub struct Button3d {
	pub pressed: bool,
	pub focused: bool,
	pub hovered: bool,
}

#[derive(Clone, Debug, Bundle)]
pub struct ButtonBuilder<M: Material = StandardMaterial> {
	pub state: Button3d,
	pub shape: WidgetShape,
	pub material: Handle<M>,
	pub transform: Transform,
	pub global_transform: GlobalTransform,
	pub visibility: Visibility,
	pub inherited_visibility: InheritedVisibility,
	pub view_visibility: ViewVisibility,
	pub layers: RenderLayers,
}

impl<M: Material> Default for ButtonBuilder<M> {
	fn default() -> Self {
		Self {
			state: default(),
			shape: default(),
			material: default(),
			transform: default(),
			global_transform: default(),
			visibility: default(),
			inherited_visibility: default(),
			view_visibility: default(),
			layers: GLOBAL_UI_RENDER_LAYERS,
		}
	}
}
