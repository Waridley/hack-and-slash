use super::in_map::icons::{Icon, IconBundleBuilder};
use crate::ui::GLOBAL_UI_RENDER_LAYERS;
use crate::util::ZUp;
use bevy::render::mesh::MeshVertexAttribute;
use bevy::render::mesh::PrimitiveTopology::TriangleList;
use bevy::render::render_asset::RenderAssetUsages;
use bevy::render::view::RenderLayers;
use bevy::utils::CowArc;
use bevy::{ecs::system::EntityCommands, prelude::*};
use bevy_svg::prelude::Origin;
use meshtext::error::MeshTextError;
use meshtext::{MeshGenerator, MeshText, OwnedFace, TextSection};
use parry2d::shape::SharedShape;

#[derive(Asset, Deref, DerefMut, TypePath)]
pub struct Font3d(pub MeshGenerator<OwnedFace>);

#[derive(Component, Debug, Clone, Deref, DerefMut)]
pub struct WidgetShape(pub SharedShape);

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

#[derive(Clone, Debug)]
pub struct PanelBuilder {
	pub size: Vec2,
	pub material: Handle<StandardMaterial>,
	pub transform: Transform,
	pub global_transform: GlobalTransform,
	pub visibility: Visibility,
	pub inherited_visibility: InheritedVisibility,
	pub layers: RenderLayers,
}

impl Default for PanelBuilder {
	fn default() -> Self {
		Self {
			size: Vec2::ONE,
			material: default(),
			transform: default(),
			global_transform: default(),
			visibility: default(),
			inherited_visibility: default(),
			layers: GLOBAL_UI_RENDER_LAYERS,
		}
	}
}

impl PanelBuilder {
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
		let mesh = meshes.add(Rectangle::new(size.x, size.y).z_up());
		(
			WidgetBundle {
				shape: WidgetShape(SharedShape::cuboid(size.x * 0.5, size.y * 0.5)),
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

#[derive(Debug)]
pub struct IconWidgetBuilder {
	pub icon: Icon,
	pub font: Handle<Font3d>,
	pub origin: Origin,
	pub size: Vec2,
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
			size: Vec2::ONE,
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
		let widget = WidgetShape(SharedShape::cuboid(size.x * 0.5, size.y * 0.5));
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
		.build(asset_server, meshes, fonts);
		((widget, svg), text)
	}

	pub fn spawn<'a>(
		self,
		cmds: &'a mut Commands,
		asset_server: &AssetServer,
		meshes: Mut<Assets<Mesh>>,
		fonts: Mut<Assets<Font3d>>,
	) -> EntityCommands<'a> {
		let (image, text) = self.build(asset_server, meshes, fonts);
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

#[derive(Debug)]
pub struct TextBuilder {
	pub text: CowArc<'static, str>,
	pub font: Handle<Font3d>,
	pub flat: bool,
	pub material: Handle<StandardMaterial>,
	pub vertex_transform: [f32; 16],
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
			vertex_transform: [
				1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0,
			],
			transform: default(),
			global_transform: default(),
			visibility: default(),
			layers: GLOBAL_UI_RENDER_LAYERS,
		}
	}
}

impl TextBuilder {
	pub fn build(
		self,
		mut meshes: Mut<Assets<Mesh>>,
		mut fonts: Mut<Assets<Font3d>>,
	) -> Result<impl Bundle, Box<dyn MeshTextError>> {
		let Self {
			text,
			flat,
			font,
			material,
			vertex_transform,
			transform,
			global_transform,
			visibility,
			layers,
		} = self;

		let mut font = fonts.map_unchanged(|fonts| fonts.get_mut(font).unwrap());
		let MeshText { bbox, vertices } =
			font.generate_section(&text, flat, Some(&vertex_transform))?;

		let shape = WidgetShape(SharedShape::cuboid(
			bbox.size().x * 0.5,
			bbox.size().y * 0.5,
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
		let mesh = meshes.add(mesh);

		Ok((
			PbrBundle {
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

	// TODO: Prevent corrupting transform if a rotation is already applied.
	//    Maybe using a typestate pattern, or forcing these methods to build immediately.
	//    Alternatively:
	//      - Only store size, offset, and maybe rotation in builder.
	//      - Compute correct values with existing rotation
	//      - Use the Affine3A type and convert in `build`
	pub fn with_size(mut self, size: Vec3) -> Self {
		self.vertex_transform[0] = size.x;
		self.vertex_transform[5] = size.y;
		self.vertex_transform[10] = size.z;
		self
	}

	pub fn with_offset(mut self, offset: Vec3) -> Self {
		self.vertex_transform[12] = offset.x;
		self.vertex_transform[13] = offset.y;
		self.vertex_transform[14] = offset.z;
		self
	}
}
