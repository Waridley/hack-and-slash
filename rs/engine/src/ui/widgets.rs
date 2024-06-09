use crate::{
	todo_warn,
	ui::{a11y::AKNode, TextMeshCache, UiAction, UiCam, UiMat, GLOBAL_UI_RENDER_LAYERS},
	util::Prev,
};
use bevy::{
	a11y::accesskit::{NodeBuilder, Role},
	ecs::system::{EntityCommand, EntityCommands},
	prelude::*,
	render::{
		mesh::{Indices, PrimitiveTopology::TriangleList},
		render_asset::RenderAssetUsages,
		view::{Layer, RenderLayers},
	},
	utils::{CowArc, HashMap, HashSet},
};
use bevy_rapier3d::parry::shape::TypedShape;
use leafwing_input_manager::prelude::ActionState;
use meshtext::{MeshGenerator, MeshText, OwnedFace, TextSection};
use rapier3d::parry::shape::SharedShape;
use std::{
	f32::consts::PI,
	fmt::{Debug, Formatter},
	ops::ControlFlow,
	sync::Arc,
};
use bevy::pbr::ExtendedMaterial;
use web_time::Duration;
use crate::mats::fade::DitherFade;
use crate::mats::fog::DistanceDither;

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

#[macro_export]
macro_rules! node_3d {
    {$T:ident $(<$($G:ident $(: $Constraints:tt)? $(= $Def:ident)?),*>)? $(where $($W:ident: $WConstraints:tt),*)? {
	    $($fields:ident: $tys:ty),* $(,)?
    }} => {
	    #[derive(Bundle, Clone)]
	    pub struct $T$(<$($G $(: $Constraints)? $(= $Def)?),*>)? $(where $($W: $WConstraints)*)? {
		    $(pub $fields: $tys,)*
		    pub handlers: crate::ui::widgets::InteractHandlers,
				pub transform: Transform,
				pub global_transform: GlobalTransform,
				pub visibility: Visibility,
				pub inherited_visibility: InheritedVisibility,
				pub view_visibility: ViewVisibility,
				pub layers: RenderLayers,
				pub ak_node: AKNode,
	    }
    }
}

node_3d! { Node3dBundle {} }

impl Default for Node3dBundle {
	fn default() -> Self {
		Self {
			handlers: default(),
			transform: default(),
			global_transform: default(),
			visibility: default(),
			inherited_visibility: default(),
			view_visibility: default(),
			layers: GLOBAL_UI_RENDER_LAYERS,
			ak_node: NodeBuilder::new(Role::Unknown).into(),
		}
	}
}

node_3d! { WidgetBundle {
	shape: WidgetShape,
}}

impl Default for WidgetBundle {
	fn default() -> Self {
		Self {
			shape: default(),
			handlers: default(),
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

#[derive(Component, Clone, Debug)]
pub struct CuboidPanel<BM: Material = UiMat> {
	pub size: Vec3,
	pub colors: Option<CuboidFaces<RectCorners<Color>>>,
	pub borders: CuboidFaces<Vec<RectBorderDesc<BM>>>,
}

impl<BM: Material> Default for CuboidPanel<BM> {
	fn default() -> Self {
		Self {
			size: Vec3::ONE,
			colors: default(),
			borders: default(),
		}
	}
}

#[derive(Component, Default, Clone, Debug, Deref, DerefMut)]
pub struct CuboidPanelBorders(pub CuboidFaces<Vec<Entity>>);

node_3d! { CuboidPanelBundle<M: Material = UiMat, BM: Material = UiMat> {
	panel: CuboidPanel<BM>,
	material: Handle<M>,
	border_entities: CuboidPanelBorders,
}}

impl<M: Material, BM: Material> Default for CuboidPanelBundle<M, BM> {
	fn default() -> Self {
		Self {
			panel: default(),
			material: default(),
			border_entities: default(),
			handlers: default(),
			transform: default(),
			global_transform: default(),
			visibility: default(),
			inherited_visibility: default(),
			view_visibility: default(),
			layers: GLOBAL_UI_RENDER_LAYERS,
			ak_node: NodeBuilder::new(Role::Pane).into(),
		}
	}
}

impl<BM: Material> CuboidPanel<BM> {
	pub fn sync(
		mut cmds: Commands,
		mut q: Query<(Entity, &Self, &RenderLayers, &mut CuboidPanelBorders), Changed<Self>>,
		mut meshes: ResMut<Assets<Mesh>>,
	) {
		for (
			id,
			CuboidPanel {
				size,
				colors,
				borders,
			},
			layers,
			mut border_entities,
		) in &mut q
		{
			for id in border_entities.iter().flatten() {
				// Despawn old borders if changed rather than just added
				cmds.get_entity(*id).map(EntityCommands::despawn_recursive);
			}
			let mut cmds = cmds.entity(id);
			let Vec3 {
				x: hx,
				y: hy,
				z: hz,
			} = *size * 0.5;
			cmds.insert(WidgetShape(SharedShape::cuboid(hx, hy, hz)));
			let mut mesh = Cuboid::new(size.x, size.y, size.z).mesh();
			if let Some(colors) = colors {
				mesh.insert_attribute(
					Mesh::ATTRIBUTE_COLOR,
					colors
						.into_iter()
						.flatten()
						.map(Color::as_rgba_f32)
						.collect::<Vec<_>>(),
				)
			}
			let mesh = mesh.with_duplicated_vertices().with_computed_flat_normals();
			cmds.insert(meshes.add(mesh));
			cmds.with_children(|cmds| {
				**border_entities = CuboidFaces {
					front: Rectangle::new(size.x, size.z),
					back: Rectangle::new(size.x, size.z),
					left: Rectangle::new(size.y, size.z),
					right: Rectangle::new(size.y, size.z),
					top: Rectangle::new(size.x, size.y),
					bottom: Rectangle::new(size.x, size.y),
				}
				.into_iter()
				.zip(borders.iter())
				.zip(CuboidFaces::NORMALS)
				.map(|((rect, desc), norm)| {
					desc.into_iter()
						.map(|desc| {
							let RectBorderDesc {
								width,
								depth,
								dilation,
								protrusion,
								colors,
								material,
							} = desc.clone();
							let mesh = rect.border(width, depth, dilation, colors);
							cmds.spawn((
								MaterialMeshBundle {
									mesh: meshes.add(mesh),
									material,
									transform: Transform {
										translation: (norm * *size * 0.5)
											+ (norm * (depth * protrusion * 0.5)),
										rotation: Quat::from_rotation_arc(Vec3::NEG_Y, norm),
										..default()
									},
									..default()
								},
								*layers,
							))
							.id()
						})
						.collect::<Vec<_>>()
				})
				.collect();
			});
		}
	}
}

#[derive(Debug, Clone)]
pub struct RectBorderDesc<M: Material = UiMat> {
	pub width: f32,
	pub depth: f32,
	/// Grow or shrink the border proportional to `width`.
	/// - Default = `-1.0`
	pub dilation: f32,
	/// Offset the border behind (negative) or in front of (positive) the panel,
	/// proportional to `depth`.
	/// - Default = `1.01` (to avoid z-fighting)
	pub protrusion: f32,
	pub colors: Option<RectCorners<Color>>,
	pub material: Handle<M>,
}

impl<M: Material> Default for RectBorderDesc<M> {
	fn default() -> Self {
		Self {
			width: 0.25,
			depth: 0.25,
			dilation: -1.0,
			protrusion: 1.01,
			colors: default(),
			material: default(),
		}
	}
}

impl<M: Material> RectBorderDesc<M> {
	pub fn mesh_for(self, rect: Rectangle) -> Mesh {
		rect.border(self.width, self.depth, self.dilation, self.colors)
	}
}

#[derive(Component, Debug, Clone)]
pub struct CylinderPanel<BM: Material> {
	pub radius: f32,
	pub length: f32,
	pub subdivisions: usize,
	pub borders: CylinderFaces<Option<CircleBorderDesc<BM>>, Option<RectBorderDesc<BM>>>,
}

impl<BM: Material> Default for CylinderPanel<BM> {
	fn default() -> Self {
		Self {
			radius: 1.0,
			length: 1.0,
			subdivisions: 6,
			borders: default(),
		}
	}
}

node_3d! { CylinderPanelBundle<M: Material, BM: Material> {
	panel: CylinderPanel<BM>,
	material: Handle<M>,
	border_entities: CylinderPanelBorderEntities,
}}

impl<M: Material, BM: Material> Default for CylinderPanelBundle<M, BM> {
	fn default() -> Self {
		Self {
			panel: default(),
			material: default(),
			border_entities: default(),
			handlers: default(),
			transform: default(),
			global_transform: default(),
			visibility: default(),
			inherited_visibility: default(),
			view_visibility: default(),
			layers: default(),
			ak_node: NodeBuilder::new(Role::Pane).into(),
		}
	}
}

#[derive(Debug, Default, Clone)]
pub struct CylinderFaces<Ends, Sides> {
	pub top: Ends,
	pub bottom: Ends,
	pub sides: Vec<Sides>,
}

#[derive(Debug, Clone)]
pub struct CircleBorderDesc<M: Material> {
	pub subdivisions: usize,
	pub dilation: f32,
	pub width: f32,
	pub depth: f32,
	pub protrusion: f32,
	pub colors: Vec<Color>,
	pub material: Handle<M>,
}

#[derive(Component, Deref, DerefMut, Clone)]
pub struct CylinderPanelBorderEntities(pub CylinderFaces<Entity, Entity>);

impl Default for CylinderPanelBorderEntities {
	fn default() -> Self {
		Self(CylinderFaces {
			top: Entity::PLACEHOLDER,
			bottom: Entity::PLACEHOLDER,
			sides: default(),
		})
	}
}

/// The default for text and SVG should be unlit, so this ID allows setting
/// the default handle while still implementing `Default` for bundles with
/// non-Standard materials.
///
/// Any child of a branch with a `Fade` component should not use the default
/// material, since fading happens per-material instead of per-entity. This
/// may change in the future if material instancing is implemented in Bevy,
/// or we write a custom pipeline to handle fading per-entity.
pub const UNLIT_MATERIAL_ID: u128 = 142787604504081244242314226814361396251;

pub fn new_unlit_material() -> UiMat {
	ExtendedMaterial {
		extension: DitherFade::default(),
		base: ExtendedMaterial {
			extension: DistanceDither::ui(),
			base: StandardMaterial {
				base_color: Color::WHITE,
				unlit: true,
				..default()
			},
		},
	}
}

#[derive(Component, Clone, Debug)]
pub struct Text3d {
	pub text: CowArc<'static, str>,
	pub flat: bool,
	pub vertex_rotation: Quat,
	pub vertex_scale: Vec3,
}

impl Default for Text3d {
	fn default() -> Self {
		Self {
			text: default(),
			flat: true,
			vertex_rotation: Quat::from_rotation_arc(Vec3::Z, Vec3::NEG_Y),
			vertex_scale: Vec3::new(1.0, 1.0, 0.4),
		}
	}
}

node_3d! { Text3dBundle<M: Material = UiMat> {
	text_3d: Text3d,
	font: Handle<Font3d>,
	material: Handle<M>,
}}

impl<M: Material> Default for Text3dBundle<M> {
	fn default() -> Self {
		Self {
			text_3d: default(),
			font: default(),
			material: Handle::weak_from_u128(UNLIT_MATERIAL_ID),
			handlers: default(),
			transform: default(),
			global_transform: default(),
			visibility: default(),
			inherited_visibility: Default::default(),
			view_visibility: Default::default(),
			layers: GLOBAL_UI_RENDER_LAYERS,
			ak_node: NodeBuilder::new(Role::StaticText).into(),
		}
	}
}

#[derive(Asset, Deref, DerefMut, TypePath)]
pub struct Font3d(pub MeshGenerator<OwnedFace>);

impl Text3d {
	pub fn sync(
		mut cmds: Commands,
		mut q: Query<(&Text3d, &Handle<Font3d>, &mut AKNode)>,
		changed_text: Query<Entity, Changed<Text3d>>,
		mut meshes: ResMut<Assets<Mesh>>,
		mut cache: ResMut<TextMeshCache>,
		mut fonts: ResMut<Assets<Font3d>>,
		mut to_retry: Local<HashSet<Entity>>,
	) {
		let mut to_try = to_retry.drain().chain(&changed_text).collect::<Vec<_>>();
		for id in to_try {
			let Ok((this, font, mut ak_node)) = q.get_mut(id) else {
				to_retry.insert(id);
				continue;
			};
			let mut cmds = cmds.entity(id);
			let Self {
				text,
				flat,
				vertex_rotation,
				vertex_scale,
			} = &*this;

			ak_node.set_value(&**text);
			let xform =
				Mat4::from_scale_rotation_translation(*vertex_scale, *vertex_rotation, Vec3::ZERO)
					.to_cols_array();

			let xform_key = array_init::array_init(|i| xform[i].to_bits());

			if !fonts.contains(font) {
				warn!("{font:?} does not (yet) exist. Retrying next frame...");
				to_retry.insert(id);
				return;
			}
			let Some((mesh, shape)) = cache
				.entry((text.clone(), xform_key, font.clone()))
				.or_insert_with(|| {
					let mut font = fonts.reborrow().map_unchanged(|fonts| {
						fonts
							.get_mut(font)
							.expect("Font was already confirmed to exist")
					});
					let MeshText { bbox, vertices } = font
						.generate_section(
							&text,
							*flat,
							Some(
								&Mat4::from_scale_rotation_translation(
									*vertex_scale,
									*vertex_rotation,
									Vec3::ZERO,
								)
								.to_cols_array(),
							),
						)
						.map_err(|e|
							// `:?` because `GlyphTriangulationError` has a useless `Display` impl
							error!("{e:?}"))
						.ok()?;

					let half_size = Vec3::new(
						bbox.size().x * 0.5,
						// For some reason, bbox is not accounting for depth
						vertex_scale.z * 0.5,
						bbox.size().z * 0.5,
					);
					let shape =
						WidgetShape(SharedShape::cuboid(half_size.x, half_size.y, half_size.z));

					let verts = vertices
						.chunks(3)
						.map(|c| [c[0] - half_size.x, c[1], c[2] - half_size.z])
						.collect::<Vec<_>>();
					let len = verts.len();
					let mut mesh = Mesh::new(TriangleList, RenderAssetUsages::RENDER_WORLD);
					mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, verts);
					mesh.insert_attribute(Mesh::ATTRIBUTE_UV_0, vec![[0.0, 0.0]; len]);
					mesh.compute_flat_normals();
					Some((meshes.add(mesh), shape))
				})
				.clone()
			else {
				error!("Failed to generate text mesh");
				continue;
			};
			cmds.insert((mesh, shape));
		}
	}
}

#[derive(Component, Default, Debug, Copy, Clone)]
pub struct Button3d {
	pub pressed: bool,
}

node_3d! { Button3dBundle<M = UiMat>
where M: Material {
	state: Button3d,
	prev_state: Prev<Button3d>,
	shape: WidgetShape,
	mesh: Handle<Mesh>,
	material: Handle<M>,
}}

impl<M: Material> Default for Button3dBundle<M> {
	fn default() -> Self {
		Self {
			state: default(),
			prev_state: default(),
			shape: default(),
			mesh: default(),
			material: default(),
			handlers: default(),
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

impl<T> From<[T; 4]> for RectCorners<T> {
	fn from(value: [T; 4]) -> Self {
		let [top_right, top_left, bottom_left, bottom_right] = value;
		Self {
			top_right,
			top_left,
			bottom_left,
			bottom_right,
		}
	}
}

impl<T> IntoIterator for RectCorners<T> {
	type Item = T;
	type IntoIter = <[T; 4] as IntoIterator>::IntoIter;

	fn into_iter(self) -> Self::IntoIter {
		<[T; 4]>::from(self).into_iter()
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

	pub fn iter(&self) -> impl Iterator<Item = &T> {
		[
			&self.front,
			&self.back,
			&self.left,
			&self.right,
			&self.top,
			&self.bottom,
		]
		.into_iter()
	}

	pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
		[
			&mut self.front,
			&mut self.back,
			&mut self.left,
			&mut self.right,
			&mut self.top,
			&mut self.bottom,
		]
		.into_iter()
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

impl CuboidFaces<Transform> {
	pub fn origins(size: Vec3) -> Self {
		CuboidFaces::<Vec3>::NORMALS
			.into_iter()
			.map(|norm| Transform {
				translation: norm * size * 0.5,
				rotation: Quat::from_rotation_arc(Vec3::NEG_Y, norm),
				..default()
			})
			.collect()
	}
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

impl<T> From<[T; 6]> for CuboidFaces<T> {
	fn from(value: [T; 6]) -> Self {
		let [front, back, left, right, top, bottom] = value;
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

#[derive(Default, Debug, GizmoConfigGroup, Reflect)]
pub struct WidgetGizmos<const LAYER: Layer>;

pub fn draw_widget_shape_gizmos<const LAYER: Layer>(
	mut gizmos: Gizmos<WidgetGizmos<LAYER>>,
	q: Query<(
		Entity,
		&GlobalTransform,
		&WidgetShape,
		&ViewVisibility,
		&RenderLayers,
	)>,
) {
	let color = Color::PURPLE;
	for (id, xform, shape, vis, layers) in &q {
		if !**vis {
			continue;
		}
		if !gizmos.config.render_layers.intersects(layers) {
			continue;
		}
		let (scale, rot, pos) = xform.to_scale_rotation_translation();
		#[cfg(feature = "debugging")]
		if (scale.x - scale.y).abs() > 0.000001 || (scale.x - scale.z).abs() > 0.000001 {
			warn!(
				?id,
				?shape,
				?scale,
				"widgets should have a uniform scale -- only drawing using `scale.x`"
			)
		}
		shape.draw_gizmo(&mut gizmos, pos, rot, scale.x, color);
	}
}

impl WidgetShape {
	pub fn draw_gizmo<T: GizmoConfigGroup>(
		&self,
		gizmos: &mut Gizmos<T>,
		global_position: Vec3,
		rotation: Quat,
		scale: f32,
		color: Color,
	) {
		let xform = Transform {
			translation: global_position,
			rotation,
			scale: Vec3::splat(scale),
		};
		match self.as_typed_shape() {
			TypedShape::Ball(ball) => {
				gizmos.sphere(global_position, rotation, ball.radius * scale, color);
			}
			TypedShape::Cuboid(cuboid) => {
				let xform = Transform {
					scale: Vec3::from(cuboid.half_extents) * 2.0 * scale,
					..xform
				};
				gizmos.cuboid(xform, color);
			}
			TypedShape::Capsule(capsule) => {
				let a = xform * Vec3::from(capsule.segment.a);
				let b = xform * Vec3::from(capsule.segment.b);
				cylinder_gizmo(gizmos, a, b, capsule.radius * scale, color);
				let dir = (b - a).normalize();
				let a_rot = Quat::from_rotation_arc(Vec3::Z, dir);
				let b_rot = a_rot.inverse();
				gizmos.arc_3d(PI, capsule.radius * scale, a, a_rot, color);
				gizmos.arc_3d(PI, capsule.radius * scale, b, b_rot, color);
			}
			TypedShape::Segment(segment) => {
				let a = xform * Vec3::from(segment.a);
				let b = xform * Vec3::from(segment.b);
				gizmos.line(a, b, color);
			}
			TypedShape::Triangle(tri) => {
				let [a, b, c]: [Vec3; 3] = [tri.a.into(), tri.b.into(), tri.c.into()];
				let [a, b, c] = [xform * a, xform * b, xform * c];
				gizmos.line(a, b, color);
				gizmos.line(b, c, color);
				gizmos.line(c, a, color);
			}
			TypedShape::TriMesh(_) => todo_warn!("Gizmo for WidgetShape(TriMesh)"),
			TypedShape::Polyline(lines) => {
				for [ia, ib] in lines.indices() {
					let a = xform * Vec3::from(lines.vertices()[*ia as usize]);
					let b = xform * Vec3::from(lines.vertices()[*ib as usize]);
					gizmos.line(a, b, color);
				}
			}
			TypedShape::HalfSpace(_) => todo_warn!("Gizmo for WidgetShape(HalfSpace)"),
			TypedShape::HeightField(_) => {
				todo_warn!("Gizmo for WidgetShape(HeightField)")
			}
			TypedShape::Compound(_) => todo_warn!("Gizmo for WidgetShape(Compound)"),
			TypedShape::ConvexPolyhedron(_) => {
				todo_warn!("Gizmo for WidgetShape(ConvexPolyhedron)")
			}
			TypedShape::Cylinder(cylinder) => {
				let a = xform * Vec3::Y * cylinder.half_height;
				let b = xform * Vec3::NEG_Y * cylinder.half_height;
				cylinder_gizmo(gizmos, a, b, cylinder.radius * scale, color);
			}
			TypedShape::Cone(_) => todo_warn!("Gizmo for WidgetShape(Cone)"),
			TypedShape::RoundCuboid(_) => {
				todo_warn!("Gizmo for WidgetShape(RoundCuboid)")
			}
			TypedShape::RoundTriangle(_) => {
				todo_warn!("Gizmo for WidgetShape(RoundTriangle)")
			}
			TypedShape::RoundCylinder(_) => {
				todo_warn!("Gizmo for WidgetShape(RoundCylinder)")
			}
			TypedShape::RoundCone(_) => todo_warn!("Gizmo for WidgetShape(RoundCone)"),
			TypedShape::RoundConvexPolyhedron(_) => {
				todo_warn!("Gizmo for WidgetShape(RoundConvexPolyhedron)")
			}
			TypedShape::Custom(_) => todo_warn!("Gizmo for WidgetShape(Custom)"),
		}
	}
}

fn cylinder_gizmo<T: GizmoConfigGroup>(
	gizmos: &mut Gizmos<T>,
	a: Vec3,
	b: Vec3,
	r: f32,
	color: Color,
) {
	let dir = Direction3d::new(b - a)
		.expect("capsule or cylinder WidgetShape should be created with non-zero, finite length");
	gizmos.circle(a, dir, r, color);
	gizmos.circle(b, dir, r, color);
	let rot = Quat::from_rotation_arc(Vec3::Z, *dir);
	let tan = rot * Vec3::X;
	let r = tan * r;
	let a_l = a - r;
	let a_r = a + r;
	let b_l = b - r;
	let b_r = b + r;
	gizmos.line(a_l, b_l, color);
	gizmos.line(a_r, b_r, color);
}

pub type InteractHandler =
	dyn Fn(Interaction, &mut EntityCommands) -> ControlFlow<()> + Send + Sync + 'static;

#[derive(Component, Deref, DerefMut, Clone)]
pub struct InteractHandlers(pub Vec<(Arc<InteractHandler>)>);

impl Default for InteractHandlers {
	fn default() -> Self {
		Self(vec![dbg_event()])
	}
}

pub fn dbg_event() -> Arc<InteractHandler> {
	Arc::new(|ev, _| {
		match &ev.kind {
			InteractionKind::Hold(_) => trace!(?ev),
			_ => debug!(?ev),
		};
		ControlFlow::Continue(())
	})
}

pub fn on_ok(
	handler: impl Fn(&mut EntityCommands) -> ControlFlow<()> + Send + Sync + 'static,
) -> Arc<InteractHandler> {
	Arc::new(move |ev, cmds| {
		if ev
			== (Interaction {
				source: InteractionSource::Action(UiAction::Ok),
				kind: InteractionKind::Begin,
			}) {
			(&handler)(cmds)
		} else {
			ControlFlow::Continue(())
		}
	})
}

impl InteractHandlers {
	pub fn on_ok(
		handler: impl Fn(&mut EntityCommands) -> ControlFlow<()> + Send + Sync + 'static,
	) -> Self {
		Self(vec![dbg_event(), on_ok(handler)])
	}

	pub fn handle(&self, event: Interaction, cmds: &mut EntityCommands) -> ControlFlow<()> {
		self.iter().try_for_each(|handler| handler(event, cmds))
	}

	pub fn system(
		mut cmds: Commands,
		q: Query<&InteractHandlers>,
		parents: Query<&Parent>,
		global_state: Res<ActionState<UiAction>>,
		states: Query<(&ActionState<UiAction>, &RenderLayers)>,
		cams: Query<(Ref<UiCam>, &RenderLayers)>,
	) {
		for (state, layers) in
			std::iter::once((&*global_state, &GLOBAL_UI_RENDER_LAYERS)).chain(&states)
		{
			let Some((cam, _)) = cams
				.iter()
				.find(|(cam, cam_layers)| **cam_layers == *layers)
			else {
				error!("no camera for {layers:?}");
				continue;
			};
			let Some(focus) = cam.focus else { continue };
			for action in state.get_just_pressed() {
				let ev = Interaction {
					source: InteractionSource::Action(action),
					kind: InteractionKind::Begin,
				};
				propagate_interaction(&mut cmds, focus, ev, &q, &parents);
			}
			for action in state.get_pressed() {
				let data = state
					.action_data(&action)
					.expect("action is pressed ∴ ActionData exists");
				let ev = Interaction {
					source: InteractionSource::Action(action),
					kind: InteractionKind::Hold(data.timing.current_duration),
				};
				propagate_interaction(&mut cmds, focus, ev, &q, &parents);
			}
			for action in state.get_just_released() {
				let ev = Interaction {
					source: InteractionSource::Action(action),
					kind: InteractionKind::Release,
				};
				propagate_interaction(&mut cmds, focus, ev, &q, &parents);
			}
			if cam.is_changed() {
				// FIXME: If other fields are added to `UiCam`, this might create false positives
				let ev = Interaction {
					source: InteractionSource::Focus,
					kind: InteractionKind::Begin,
				};
				// Focus events don't propagate
				q.get(focus)
					.map(|handlers| handlers.handle(ev, &mut cmds.entity(focus)))
					.inspect_err(|e| error!("{e}"))
					.ok();
			}
			// FIXME: Hold/release focus events
		}
	}
}

fn propagate_interaction(
	cmds: &mut Commands,
	entity: Entity,
	event: Interaction,
	q: &Query<&InteractHandlers>,
	parents: &Query<&Parent>,
) -> ControlFlow<()> {
	q.get(entity)
		.map(|handlers| handlers.handle(event, &mut cmds.entity(entity)))
		.unwrap_or(ControlFlow::Continue(()))?;
	for entity in parents.iter_ancestors(entity) {
		q.get(entity)
			.map(|handlers| handlers.handle(event, &mut cmds.entity(entity)))
			.unwrap_or(ControlFlow::Continue(()))?;
	}
	ControlFlow::Continue(())
}

impl FromIterator<Arc<InteractHandler>> for InteractHandlers {
	fn from_iter<T: IntoIterator<Item = Arc<InteractHandler>>>(iter: T) -> Self {
		Self(iter.into_iter().collect())
	}
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum InteractionSource {
	Action(UiAction),
	/// Focus status changed.
	Focus,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum InteractionKind {
	/// Pressed button or gained focus.
	Begin,
	/// Still holding button or maintaining focus.
	Hold(Duration),
	/// Released button or lost focus.
	Release,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Interaction {
	pub source: InteractionSource,
	pub kind: InteractionKind,
}
