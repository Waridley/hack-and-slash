use crate::{
	todo_warn,
	ui::{
		a11y::AKNode,
		text::{Tessellator, TextMeshCache},
		widgets::borders::Border,
		MenuStack, UiAction, UiMat, UiMatBuilder, GLOBAL_UI_RENDER_LAYERS,
	},
};
use atomicow::CowArc;
use bevy::{
	a11y::AccessibilityNode,
	color::palettes::basic::PURPLE,
	ecs::system::EntityCommands,
	prelude::*,
	render::{
		mesh::{Indices, MeshAabb, PrimitiveTopology::TriangleList, VertexAttributeValues},
		render_asset::RenderAssetUsages,
		view::{Layer, RenderLayers},
	},
	utils::HashSet,
};
use bevy_rapier3d::parry::{
	math::{Isometry, Vector},
	shape::TypedShape,
};
use leafwing_input_manager::{action_state::ActionKindData, prelude::ActionState};
use lyon_tessellation::VertexBuffers;
use rapier3d::parry::shape::SharedShape;
use serde::{Deserialize, Serialize};
use smallvec::{smallvec, SmallVec};
use std::{
	borrow::Cow,
	f32::consts::PI,
	fmt::{Debug, Formatter},
	ops::ControlFlow,
	sync::Arc,
};
use tiny_bail::prelude::r;
use web_time::Duration;

pub mod borders;

#[derive(Component, Clone, Deref, DerefMut)]
#[require(Node3d)]
pub struct WidgetShape {
	#[deref]
	pub shape: SharedShape,
	pub isometry: Isometry<f32>,
}

impl Default for WidgetShape {
	fn default() -> Self {
		Self {
			shape: SharedShape::cuboid(0.5, 0.5, 0.5),
			isometry: default(),
		}
	}
}

impl Debug for WidgetShape {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		ron::to_string(&self.shape).fmt(f)
	}
}

pub fn offset_mesh_positions(mesh: &mut Mesh, translation: Vec3, rotation: Quat) {
	let VertexAttributeValues::Float32x3(positions) = mesh
		.attribute_mut(Mesh::ATTRIBUTE_POSITION)
		.expect("Cuboid mesh has positions")
	else {
		unreachable!()
	};
	for pos in positions {
		*pos = ((rotation * Vec3::from_array(*pos)) + translation).to_array();
	}
}

#[derive(Component, Default, Debug, Clone)]
#[require(
	crate::ui::interact::InteractHandlers,
	Transform,
	GlobalTransform,
	Visibility,
	InheritedVisibility,
	ViewVisibility,
	RenderLayers(|| crate::ui::GLOBAL_UI_RENDER_LAYERS),
	crate::ui::focus::AdjacentWidgets,
	AKNode(|| AccessibilityNode(accesskit::Node::new(::accesskit::Role::Unknown))),
)]
pub struct Node3d;

#[derive(Component, Default, Clone, Debug, Reflect)]
#[require(WidgetShape)]
#[reflect(Component)]
pub struct Panel;

#[derive(Bundle, Default, Debug, Clone)]
pub struct PanelBundle<M: Material = UiMat> {
	pub panel: Panel,
	pub shape: WidgetShape,
	pub mesh: Mesh3d,
	pub material: MeshMaterial3d<M>,
}

#[derive(Component, Clone, Debug, Reflect)]
#[require(Node3d)]
#[reflect(Component)]
pub struct CuboidPanel {
	pub size: Vec3,
	pub colors: Option<CuboidFaces<RectCorners<Srgba>>>,
	pub vertex_translation: Vec3,
	pub vertex_rotation: Quat,
	pub mesh_margin: Vec3,
}

impl Default for CuboidPanel {
	fn default() -> Self {
		Self {
			size: Vec3::ONE,
			colors: default(),
			vertex_translation: default(),
			vertex_rotation: default(),
			mesh_margin: default(),
		}
	}
}

impl CuboidPanel {
	pub fn sync_mesh(
		mut cmds: Commands,
		mut q: Query<(Entity, &Self), Changed<Self>>,
		mut meshes: ResMut<Assets<Mesh>>,
	) {
		for (
			id,
			CuboidPanel {
				size,
				colors,
				vertex_translation: translation,
				vertex_rotation: rotation,
				mesh_margin,
			},
		) in &mut q
		{
			let mut cmds = cmds.entity(id);
			let Vec3 {
				x: hx,
				y: hy,
				z: hz,
			} = *size * 0.5;
			let pos = Vector::from(*translation);
			cmds.insert(WidgetShape {
				shape: SharedShape::cuboid(hx, hy, hz),
				isometry: Isometry::new(pos, rotation.to_scaled_axis().into()),
			});

			let mut mesh = Cuboid::new(
				size.x - mesh_margin.x,
				size.y - mesh_margin.y,
				size.z - mesh_margin.z,
			)
			.mesh()
			.build();
			offset_mesh_positions(&mut mesh, *translation, *rotation);
			if let Some(colors) = colors {
				mesh.insert_attribute(
					Mesh::ATTRIBUTE_COLOR,
					colors
						.into_iter()
						.flatten()
						.map(Srgba::to_f32_array)
						.collect::<Vec<_>>(),
				)
			}
			let mesh = mesh.with_duplicated_vertices().with_computed_flat_normals();
			let aabb = mesh.compute_aabb().unwrap();
			cmds.insert((Mesh3d(meshes.add(mesh)), aabb));
		}
	}
}

#[derive(Component, Debug, Clone, Reflect)]
#[require(Node3d)]
#[reflect(Component)]
pub struct CylinderPanel {
	/// Circumradius of the regular polygon defining the cross-section
	/// of the cylinder's mesh.
	pub radius: f32,
	pub length: f32,
	pub subdivisions: u32,
	pub translation: Vec3,
	pub rotation: Quat,
}

impl Default for CylinderPanel {
	fn default() -> Self {
		Self {
			radius: 1.0,
			length: 1.0,
			subdivisions: 6,
			translation: default(),
			rotation: default(),
		}
	}
}

impl CylinderPanel {
	pub fn sync_mesh(
		mut cmds: Commands,
		q: Query<(Entity, &Self), Changed<Self>>,
		mut meshes: ResMut<Assets<Mesh>>,
	) {
		for (
			id,
			Self {
				radius,
				length,
				subdivisions,
				translation,
				rotation,
			},
		) in &q
		{
			let builder = Cylinder {
				radius: *radius,
				half_height: *length * 0.5,
			}
			.mesh()
			.resolution(*subdivisions);
			debug!(?builder);
			let mut mesh = Mesh::from(builder)
				.with_duplicated_vertices()
				.with_computed_flat_normals();
			offset_mesh_positions(&mut mesh, *translation, *rotation);
			mesh.asset_usage = RenderAssetUsages::RENDER_WORLD;
			let handle = meshes.add(mesh);
			cmds.entity(id).insert(Mesh3d(handle.clone()));
			let mesh = meshes.get(&handle);
			debug!(?handle, ?mesh);
		}
	}
}

#[derive(Debug, Default, Clone, Reflect)]
pub struct CylinderFaces<Ends, Sides> {
	pub top: Ends,
	pub bottom: Ends,
	pub sides: Vec<Sides>,
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
	UiMatBuilder {
		std: StandardMaterial {
			base_color: Color::WHITE,
			unlit: true,
			alpha_mode: AlphaMode::Mask(0.5),
			..default()
		},
		..default()
	}
	.into()
}

#[derive(Component, Clone, Debug, Reflect)]
#[require(Node3d)] // WidgetShape is automatically generated
pub struct Text3d {
	pub font: Handle<Font>,
	pub text: Cow<'static, str>,
	pub flat: bool,
	pub vertex_translation: Vec3,
	pub vertex_rotation: Quat,
	pub vertex_scale: Vec3,
	pub align_origin: Vec3,
	pub color: Color,
	pub tolerance: f32,
	pub usages: RenderAssetUsages,
}

impl Default for Text3d {
	fn default() -> Self {
		Self {
			text: default(),
			font: default(),
			flat: true,
			vertex_translation: Vec3::ZERO,
			vertex_rotation: Quat::IDENTITY,
			vertex_scale: Vec3::ONE,
			align_origin: Vec3::splat(0.5),
			color: Color::WHITE,
			tolerance: 0.002,
			usages: default(),
		}
	}
}

impl Text3d {
	pub fn sync_mesh(
		mut cmds: Commands,
		mut q: Query<(&Text3d, &mut AKNode)>,
		changed_text: Query<Entity, Changed<Text3d>>,
		mut meshes: ResMut<Assets<Mesh>>,
		mut cache: ResMut<TextMeshCache>,
		fonts: Res<Assets<Font>>,
		mut to_retry: Local<HashSet<Entity>>,
		mut tessellator: ResMut<Tessellator>,
	) {
		let to_try = to_retry.drain().chain(&changed_text).collect::<Vec<_>>();
		for id in to_try {
			let (this, mut ak_node) = match q.get_mut(id) {
				Ok(item) => item,
				Err(e) => {
					error!("Couldn't query for changed text entity: {e}");
					to_retry.insert(id);
					continue;
				}
			};
			let mut cmds = cmds.entity(id);
			let Self {
				ref text,
				ref font,
				flat,
				vertex_translation,
				vertex_rotation,
				vertex_scale,
				align_origin,
				color,
				tolerance,
				usages,
			} = *this;

			ak_node.set_value(&**text);
			let xform = Mat4::from_scale_rotation_translation(
				vertex_scale,
				vertex_rotation,
				vertex_translation,
			)
			.to_cols_array();

			let xform_key = array_init::array_init(|i| xform[i].to_bits());

			if !fonts.contains(font) {
				warn!("{font:?} does not (yet) exist. Retrying next frame...");
				to_retry.insert(id);
				continue;
			}
			let Some((mesh, shape)) = cache
				.entry((text.clone(), xform_key, font.clone(), flat))
				.or_insert_with(|| {
					let font = fonts
						.get(font)
						.expect("Font was already confirmed to exist");

					let (
						VertexBuffers {
							vertices,
							mut indices,
						},
						bbox,
					) = r!(tessellator.tessellate(text, font, tolerance, vertex_scale.xz()));

					if vertices.is_empty() {
						return Some((meshes.add(Rectangle::default()), WidgetShape::default()));
					}

					let half_size = Vec3::new(
						bbox.size().x * 0.5,
						vertex_scale.y * 0.5,
						bbox.size().y * 0.5,
					);
					let center = bbox.center();
					let origin = Vec3::new(
						bbox.size().x * align_origin.x,
						vertex_scale.y * (align_origin.y - 0.5),
						bbox.size().y * align_origin.z,
					) - vertex_translation;
					let shape = WidgetShape {
						shape: SharedShape::cuboid(half_size.x, half_size.y, half_size.z),
						isometry: Isometry::new(
							Vector::new(center.x - origin.x, 0.0 - origin.y, center.y - origin.z),
							default(),
						),
					};

					let mut verts = vertices
						.iter()
						.map(|c| {
							let c = Vec3::new(c.x, 0.0, c.y);
							let c = vertex_rotation * c;
							let c = c - origin;
							c.to_array()
						})
						.collect::<Vec<_>>();

					if !flat {
						let len = r!(u32::try_from(vertices.len()));
						verts.extend(vertices.into_iter().map(|c| {
							let c = Vec3::new(c.x, vertex_scale.y, c.y);
							let c = vertex_rotation * c;
							let c = c - origin;
							c.to_array()
						}));
						let edges = || {
							indices
								.chunks(3)
								.flat_map(|tri| {
									[[tri[0], tri[1]], [tri[1], tri[2]], [tri[2], tri[0]]]
								})
								.enumerate()
						};
						let mut new_indices = indices
							.chunks(3)
							.flat_map(|front| {
								[front[0] + len, front[2] + len, front[1] + len]
							})
							.collect::<Vec<_>>();
						for (i, edge) in edges() {
							let mut shared = false;
							for (j, [a, b]) in edges() {
								if i == j {
									continue;
								} else if edge == [a, b] || edge == [b, a] {
									shared = true;
									break;
								}
							}
							if !shared {
								let [a, b] = edge;
								let [c, d] = [a + len, b + len];
								new_indices.extend([b, a, c, b, c, d]);
							}
						}
						indices.extend(new_indices);
					}

					let len = verts.len();
					let indices = if u16::try_from(len).is_ok() {
						Indices::U16(indices.into_iter().map(|i| i as u16).collect())
					} else {
						if text.len() < 4 {
							warn!("Mesh indices for short string \"{text}\" do not fit in u16. Try increasing tolerance.");
						}
						Indices::U32(indices)
					};

					let mut mesh = Mesh::new(TriangleList, usages);
					mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, verts);
					mesh.insert_indices(indices);
					mesh.insert_attribute(Mesh::ATTRIBUTE_UV_0, vec![[0.0, 0.0]; len]);
					mesh.insert_attribute(
						Mesh::ATTRIBUTE_COLOR,
						vec![color.to_linear().to_f32_array(); len],
					);
					if !flat {
						mesh.duplicate_vertices();
						mesh.compute_flat_normals();
					} else {
						mesh.insert_attribute(
							Mesh::ATTRIBUTE_NORMAL,
							vec![vertex_rotation * Vec3::NEG_Y; len],
						);
					}
					Some((meshes.add(mesh), shape))
				})
				.clone()
			else {
				error!(?text, "Failed to generate text mesh");
				continue;
			};
			debug!(?id, ?mesh, ?shape);
			cmds.insert((Mesh3d(mesh), shape));
		}
	}
}

#[derive(Copy, Clone, Default, Debug, Reflect)]
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

#[derive(Copy, Clone, Default, Debug, Reflect)]
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
	let color = PURPLE;
	for (_id, xform, shape, vis, layers) in &q {
		if !**vis {
			continue;
		}
		if !gizmos.config.render_layers.intersects(layers) {
			continue;
		}
		let (scale, rot, pos) = xform.to_scale_rotation_translation();
		#[cfg(debug_assertions)]
		if (scale.x - scale.y).abs() > 0.000001 || (scale.x - scale.z).abs() > 0.000001 {
			warn!(
				?_id,
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
		color: impl Into<Color>,
	) {
		let translation = global_position + Vec3::from(self.isometry.translation);
		let rotation = rotation * Quat::from(self.isometry.rotation);
		let xform = Transform {
			translation,
			rotation,
			scale: Vec3::splat(scale),
		};
		match self.as_typed_shape() {
			TypedShape::Ball(ball) => {
				let iso = Isometry3d {
					translation: translation.into(),
					rotation,
				};
				gizmos.sphere(iso, ball.radius * scale, color);
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
				let color = color.into();
				cylinder_gizmo(gizmos, a, b, capsule.radius * scale, color);
				let dir = (b - a).normalize();
				let a_rot = Quat::from_rotation_arc(Vec3::Z, dir);
				let b_rot = a_rot.inverse();
				gizmos.arc_3d(
					PI,
					capsule.radius * scale,
					Isometry3d {
						translation: a.into(),
						rotation: a_rot,
					},
					color,
				);
				gizmos.arc_3d(
					PI,
					capsule.radius * scale,
					Isometry3d {
						translation: b.into(),
						rotation: b_rot,
					},
					color,
				);
			}
			TypedShape::Segment(segment) => {
				let a = xform * Vec3::from(segment.a);
				let b = xform * Vec3::from(segment.b);
				gizmos.line(a, b, color);
			}
			TypedShape::Triangle(tri) => {
				let [a, b, c]: [Vec3; 3] = [tri.a.into(), tri.b.into(), tri.c.into()];
				let [a, b, c] = [xform * a, xform * b, xform * c];
				let color = color.into();
				gizmos.line(a, b, color);
				gizmos.line(b, c, color);
				gizmos.line(c, a, color);
			}
			TypedShape::TriMesh(_) => todo_warn!("Gizmo for WidgetShape(TriMesh)"),
			TypedShape::Polyline(lines) => {
				let color = color.into();
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
				cylinder_gizmo(gizmos, a, b, cylinder.radius * scale, color.into());
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
	let dir = Dir3::new(b - a)
		.expect("capsule or cylinder WidgetShape should be created with non-zero, finite length");
	let rot = Quat::from_rotation_arc(Vec3::Z, *dir);
	gizmos.circle(
		Isometry3d {
			translation: a.into(),
			rotation: rot,
		},
		r,
		color,
	);
	gizmos.circle(
		Isometry3d {
			translation: b.into(),
			rotation: rot,
		},
		r,
		color,
	);
	let tan = rot * Vec3::X;
	let r = tan * r;
	let a_l = a - r;
	let a_r = a + r;
	let b_l = b - r;
	let b_r = b + r;
	gizmos.line(a_l, b_l, color);
	gizmos.line(a_r, b_r, color);
}

#[derive(Component, Debug, Copy, Clone, Reflect, Deref, DerefMut)]
#[reflect(Component)]
pub struct PrevFocus(pub Entity);

impl Default for PrevFocus {
	fn default() -> Self {
		Self(Entity::PLACEHOLDER)
	}
}

#[derive(Component, Copy, Clone, Debug, Reflect, Serialize, Deserialize)]
#[require(Node3d)]
#[reflect(Component, Serialize, Deserialize)]
pub struct CuboidContainer {
	pub size: Vec3,
	pub translation: Vec3,
	pub rotation: Quat,
}

impl CuboidContainer {
	pub fn sync_shape(mut cmds: Commands, q: Query<(Entity, &Self), Changed<Self>>) {
		for (id, this) in &q {
			debug!(?id, "Syncing WidgetShape");
			let h_size = this.size * 0.5;
			let pos = Vector::from(this.translation);
			cmds.entity(id).insert(WidgetShape {
				shape: SharedShape::cuboid(h_size.x, h_size.y, h_size.z),
				isometry: Isometry::new(pos, this.rotation.to_scaled_axis().into()),
			});
		}
	}
}

impl Default for CuboidContainer {
	fn default() -> Self {
		Self {
			size: Vec3::ONE,
			translation: default(),
			rotation: default(),
		}
	}
}
