use super::in_map::icons::{Icon, IconBundleBuilder};
use crate::{
	ui::{a11y::AKNode, TextMeshCache, GLOBAL_UI_RENDER_LAYERS},
	util::Prev,
};
use bevy::{
	a11y::accesskit::{NodeBuilder, Role},
	ecs::system::EntityCommands,
	prelude::*,
	render::{
		mesh::{Indices, PrimitiveTopology, PrimitiveTopology::TriangleList},
		render_asset::RenderAssetUsages,
		view::RenderLayers,
	},
	utils::CowArc,
};
use bevy_svg::prelude::Origin;
use meshtext::{MeshGenerator, MeshText, OwnedFace, TextSection};
use rapier3d::parry::shape::SharedShape;
use std::fmt::{Debug, Formatter};

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

#[derive(Bundle, Clone)]
pub struct WidgetBundle {
	pub shape: WidgetShape,
	pub transform: Transform,
	pub global_transform: GlobalTransform,
	pub visibility: Visibility,
	pub inherited_visibility: InheritedVisibility,
	pub view_visibility: ViewVisibility,
	pub layers: RenderLayers,
	pub ak_node: AKNode,
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
			ak_node: NodeBuilder::new(default()).into(),
		}
	}
}

#[derive(Clone, Debug)]
pub struct PanelBuilder<M: Material> {
	pub size: Vec3,
	pub material: Handle<M>,
	pub colors: Option<CuboidFaces<Color>>,
	pub borders: CuboidFaces<Option<RectBorderDesc>>,
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
			colors: default(),
			borders: default(),
			transform: default(),
			global_transform: default(),
			visibility: default(),
			inherited_visibility: default(),
			layers: GLOBAL_UI_RENDER_LAYERS,
		}
	}
}

impl<M: Material> PanelBuilder<M> {
	pub fn build(self, meshes: &mut Assets<Mesh>) -> (impl Bundle, CuboidFaces<Option<Mesh>>) {
		let Self {
			size,
			material,
			colors,
			borders,
			transform,
			global_transform,
			visibility,
			inherited_visibility,
			layers,
		} = self;
		let mut mesh = Cuboid::new(size.x, size.y, size.z).mesh();
		if let Some(colors) = colors {
			mesh.insert_attribute(
				Mesh::ATTRIBUTE_COLOR,
				colors
					.into_iter()
					.flat_map(|color| [color.as_rgba_f32(); 4])
					.collect::<Vec<_>>(),
			)
		}
		let mesh = mesh.with_duplicated_vertices().with_computed_flat_normals();

		let borders = <[Option<RectBorderDesc>; 6]>::from(borders);
		let border_meshes = CuboidFaces::new(
			borders[0].map(|desc| desc.mesh_for(Rectangle::new(size.x, size.z))),
			borders[1].map(|desc| desc.mesh_for(Rectangle::new(size.x, size.z))),
			borders[2].map(|desc| desc.mesh_for(Rectangle::new(size.y, size.z))),
			borders[3].map(|desc| desc.mesh_for(Rectangle::new(size.y, size.z))),
			borders[4].map(|desc| desc.mesh_for(Rectangle::new(size.x, size.y))),
			borders[5].map(|desc| desc.mesh_for(Rectangle::new(size.x, size.y))),
		);

		let mesh = meshes.add(mesh);
		(
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
					ak_node: NodeBuilder::new(Role::Pane).into(),
				},
				mesh,
				material,
			),
			border_meshes,
		)
	}
}

#[derive(Debug, Copy, Clone)]
pub struct RectBorderDesc {
	pub width: f32,
	pub depth: f32,
	pub dilation: f32,
	pub colors: Option<RectCorners<Color>>,
}

impl Default for RectBorderDesc {
	fn default() -> Self {
		Self {
			width: 0.5,
			depth: 0.5,
			dilation: 0.0,
			colors: default(),
		}
	}
}

impl RectBorderDesc {
	pub fn mesh_for(self, rect: Rectangle) -> Mesh {
		rect.border(self.width, self.depth, self.dilation, self.colors)
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

#[derive(Asset, Deref, DerefMut, TypePath)]
pub struct Font3d(pub MeshGenerator<OwnedFace>);

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

		let xform = Mat4::from_scale_rotation_translation(
			vertex_scale,
			vertex_rotation,
			vertex_translation,
		)
		.to_cols_array();

		let xform_key = array_init::array_init(|i| xform[i].to_bits());

		let (mesh, shape) = cache
			.entry((text.clone(), xform_key, font.clone()))
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
			AKNode::from(NodeBuilder::new(Role::StaticText)),
		))
	}
}

#[derive(Component, Default, Debug, Copy, Clone)]
pub struct Button3d {
	pub pressed: bool,
	pub focused: bool,
	pub hovered: bool,
}

#[derive(Clone, Bundle)]
pub struct Button3dBundle<M: Material = StandardMaterial> {
	pub state: Button3d,
	pub prev_state: Prev<Button3d>,
	pub shape: WidgetShape,
	pub material: Handle<M>,
	pub transform: Transform,
	pub global_transform: GlobalTransform,
	pub visibility: Visibility,
	pub inherited_visibility: InheritedVisibility,
	pub view_visibility: ViewVisibility,
	pub layers: RenderLayers,
	pub ak_node: AKNode,
}

impl<M: Material> Default for Button3dBundle<M> {
	fn default() -> Self {
		Self {
			state: default(),
			prev_state: default(),
			shape: default(),
			material: default(),
			transform: default(),
			global_transform: default(),
			visibility: default(),
			inherited_visibility: default(),
			view_visibility: default(),
			layers: GLOBAL_UI_RENDER_LAYERS,
			ak_node: NodeBuilder::new(Role::Button).into(),
		}
	}
}

pub trait BorderMesh {
	type Colors;

	/// `width`: the thickness of the border when viewed straight-on.
	/// `depth`: How far the border sticks out from the face of the rectangle.
	/// `dilation`: `-1.0` = an internal border. `1.0` = an external border.
	fn border(&self, width: f32, depth: f32, dilation: f32, colors: Option<Self::Colors>) -> Mesh;
}

impl BorderMesh for Rectangle {
	type Colors = RectCorners<Color>;

	fn border(&self, width: f32, depth: f32, dilation: f32, colors: Option<Self::Colors>) -> Mesh {
		let tr = self.half_size;
		let tl = Vec2::new(-tr.x, tr.y);
		let bl = -tr;
		let br = Vec2::new(tr.x, -tr.y);
		let positions = [tr, tl, bl, br]
			.into_iter()
			.flat_map(|Vec2 { x, y }| {
				let xs = x.signum();
				let ys = y.signum();
				let o_offset = width * (dilation + 1.0) * 0.5;
				let outer = Vec2::new(x + (xs * o_offset), y + (ys * o_offset));
				let i_offset = width * (dilation - 1.0) * 0.5;
				let inner = Vec2::new(x + (xs * i_offset), y + (ys * i_offset));
				[
					[inner.x, -depth * 0.5, inner.y],
					[outer.x, -depth * 0.5, outer.y],
					[outer.x, depth * 0.5, outer.y],
					[inner.x, depth * 0.5, inner.y],
				]
			})
			.collect::<Vec<_>>();

		let indices = [0, 1, 2, 3, 0]
			.windows(2)
			.flat_map(|vert_pair| {
				[0, 1, 2, 3, 0].windows(2).flat_map(|corner_pair| {
					[
						(corner_pair[0] * 4) + vert_pair[0],
						(corner_pair[0] * 4) + vert_pair[1],
						(corner_pair[1] * 4) + vert_pair[0],
						(corner_pair[1] * 4) + vert_pair[0],
						(corner_pair[0] * 4) + vert_pair[1],
						(corner_pair[1] * 4) + vert_pair[1],
					]
				})
			})
			.collect::<Vec<_>>();

		let mut mesh = Mesh::new(TriangleList, RenderAssetUsages::RENDER_WORLD)
			.with_inserted_attribute(Mesh::ATTRIBUTE_POSITION, positions);

		if let Some(colors) = colors {
			use std::iter::repeat;
			let colors: [Color; 4] = colors.into();
			let colors = repeat(colors[0].as_rgba_f32())
				.take(4)
				.chain(repeat(colors[1].as_rgba_f32()).take(4))
				.chain(repeat(colors[2].as_rgba_f32()).take(4))
				.chain(repeat(colors[3].as_rgba_f32()).take(4))
				.collect::<Vec<_>>();

			mesh.insert_attribute(Mesh::ATTRIBUTE_COLOR, colors);
		}

		mesh.with_inserted_indices(Indices::U16(indices))
			.with_duplicated_vertices()
			.with_computed_flat_normals()
	}
}

#[derive(Copy, Clone, Default, Debug)]
pub struct RectCorners<T> {
	pub top_right: T,
	pub top_left: T,
	pub bottom_left: T,
	pub bottom_right: T,
}

impl<T> RectCorners<T> {
	pub fn new(top_left: T, top_right: T, bottom_left: T, bottom_right: T) -> Self {
		Self {
			top_right,
			top_left,
			bottom_left,
			bottom_right,
		}
	}
}

impl<T> From<RectCorners<T>> for [T; 4] {
	fn from(value: RectCorners<T>) -> Self {
		[
			value.top_right,
			value.top_left,
			value.bottom_left,
			value.bottom_right,
		]
	}
}

#[derive(Copy, Clone, Default, Debug)]
pub struct CuboidCorners<T> {
	pub front_top_right: T,
	pub front_top_left: T,
	pub front_bottom_left: T,
	pub front_bottom_right: T,
	pub back_top_right: T,
	pub back_top_left: T,
	pub back_bottom_left: T,
	pub back_bottom_right: T,
}

impl<T> CuboidCorners<T> {
	pub fn new(
		front_top_left: T,
		front_top_right: T,
		front_bottom_left: T,
		front_bottom_right: T,
		back_top_left: T,
		back_top_right: T,
		back_bottom_left: T,
		back_bottom_right: T,
	) -> Self {
		Self {
			front_top_right,
			front_top_left,
			front_bottom_left,
			front_bottom_right,
			back_top_right,
			back_top_left,
			back_bottom_left,
			back_bottom_right,
		}
	}
}

impl<T> From<CuboidCorners<T>> for [T; 8] {
	fn from(value: CuboidCorners<T>) -> Self {
		[
			value.front_top_right,
			value.front_top_left,
			value.front_bottom_left,
			value.front_bottom_right,
			value.back_top_right,
			value.back_top_left,
			value.back_bottom_left,
			value.back_bottom_right,
		]
	}
}

#[derive(Copy, Clone, Default, Debug)]
pub struct CuboidFaces<T> {
	pub front: T,
	pub back: T,
	pub left: T,
	pub right: T,
	pub top: T,
	pub bottom: T,
}

impl<T> CuboidFaces<T> {
	pub fn new(front: T, back: T, left: T, right: T, top: T, bottom: T) -> Self {
		Self {
			front,
			back,
			left,
			right,
			top,
			bottom,
		}
	}
}

impl CuboidFaces<Vec3> {
	pub const NORMALS: Self = Self {
		front: Vec3::NEG_Y,
		back: Vec3::Y,
		left: Vec3::NEG_X,
		right: Vec3::X,
		top: Vec3::Z,
		bottom: Vec3::NEG_Z,
	};
}

impl<T> From<CuboidFaces<T>> for [T; 6] {
	fn from(value: CuboidFaces<T>) -> Self {
		[
			value.front,
			value.back,
			value.left,
			value.right,
			value.top,
			value.bottom,
		]
	}
}

impl<T> IntoIterator for CuboidFaces<T> {
	type Item = T;
	type IntoIter = <[T; 6] as IntoIterator>::IntoIter;

	fn into_iter(self) -> Self::IntoIter {
		<[T; 6]>::from(self).into_iter()
	}
}

impl<F> FromIterator<F> for CuboidFaces<F> {
	fn from_iter<T: IntoIterator<Item = F>>(iter: T) -> Self {
		let mut iter = iter.into_iter();
		Self {
			front: iter.next().unwrap(),
			back: iter.next().unwrap(),
			left: iter.next().unwrap(),
			right: iter.next().unwrap(),
			top: iter.next().unwrap(),
			bottom: iter.next().unwrap(),
		}
	}
}
