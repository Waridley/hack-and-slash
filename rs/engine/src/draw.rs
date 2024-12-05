//! Retained-mode drawing utilities.

use bevy::{
	prelude::*,
	render::{
		mesh::{Indices, PrimitiveTopology, VertexAttributeValues},
		render_asset::RenderAssetUsages,
	},
};
use std::f32::consts::{FRAC_PI_2, PI, TAU};
use smallvec::{smallvec, SmallVec};

/// The corners of a square with dimensions [1.0, 1.0], centered around the origin.
#[inline]
pub fn unit_square_points() -> SmallVec<[Vec2; 4]> {
	square_points(1.0)
}

/// The corners of a square with full width and height `width`, centered around the origin.
#[inline]
pub fn square_points(width: f32) -> SmallVec<[Vec2; 4]> {
	rect_points(width, width)
}

/// The corners of a rectangle centered around the origin.
#[inline]
pub fn rect_points(width: f32, height: f32) -> SmallVec<[Vec2; 4]> {
	rect_points_offset(width, height, Vec2::ZERO)
}

/// The corners of a rectangle centered around `offset`.
#[inline]
pub fn rect_points_offset(width: f32, height: f32, offset: Vec2) -> SmallVec<[Vec2; 4]> {
	let w = width * 0.5;
	let h = height * 0.5;
	let x = offset.x;
	let y = offset.y;
	smallvec![
		Vec2::new(w + x, h + y),
		Vec2::new(-w + x, h + y),
		Vec2::new(-w + x, -h + y),
		Vec2::new(w + x, -h + y),
	]
}

/// Coordinates of the vertices of a regular polygon centered on the origin.
///
/// `sides`: Number of sides (and vertices) of the shape.
/// `radius`: The radius of the shape at each vertex.
/// `clocking` The rotation of the shape in terms of a fraction
///     of the angle between two vertices, i.e., `0.0` will align
///     a vertex with `Vec2::X`, while `0.5` will align an edge
///     with `Vec2::X` instead.
pub fn polygon_points(sides: u32, radius: f32, clocking: f32) -> SmallVec<[Vec2; 4]> {
	let wedge = TAU / sides as f32;
	(0..sides)
		.map(|angle| {
			let angle = wedge * angle as f32;
			Vec2::from_angle(angle + (clocking * wedge)) * radius
		})
		.collect()
}

/// A "two-dimensional" sequence of lines, drawn using a 3D Mesh by extruding the
/// cross-section.
#[derive(Debug, Clone)]
pub struct PlanarPolyLine {
	/// The points defining the shape, at the origin of [cross_section].
	///
	/// Defaults to [unit_square_points].
	pub points: SmallVec<[Vec2; 4]>,
	/// The vertices defining the cross-section shape, with the origin at each
	/// point in [points]. The last vertex will be connected to the first.
	///
	/// Defaults to a square of size [0.25, 0.25], centered around [points].
	pub cross_section: SmallVec<[Vec2; 4]>,
	/// The colors of each vertex in the mesh.
	///
	/// The inner `Vec`s correspond to the points of [cross_section] for each point
	/// in [points].
	///
	/// If there are more vertices in [cross_section] than any given inner `Vec`, the last color
	/// will be repeated for the remaining vertices.
	/// Also, if any inner `Vec` is empty, the last color from the previous one will be used.
	/// If the outer `Vec` is shorter than `points`, the last color will also be repeated for
	/// all remaining vertices.
	pub colors: SmallVec<[SmallVec<[LinearRgba; 1]>; 2]>,
	/// If `true`, the last point will be connected to the first, closing the shape.
	///
	/// Defaults to `true`.
	pub closed: bool,
}

impl Default for PlanarPolyLine {
	fn default() -> Self {
		Self {
			points: unit_square_points(),
			cross_section: square_points(0.25),
			colors: SmallVec::new(),
			closed: true,
		}
	}
}

/// Construct a `PlanarPolyLine` with a square cross-section and no vertex colors.
impl FromIterator<Vec2> for PlanarPolyLine {
	fn from_iter<T: IntoIterator<Item = Vec2>>(iter: T) -> Self {
		Self {
			points: iter.into_iter().collect(),
			..default()
		}
	}
}

/// Construct a `PlanarPolyLine` with a square cross-section and a single
/// color at each corner.
impl FromIterator<(Vec2, LinearRgba)> for PlanarPolyLine {
	fn from_iter<T: IntoIterator<Item = (Vec2, LinearRgba)>>(iter: T) -> Self {
		let (points, colors) = iter
			.into_iter()
			.map(|(point, color)| (point, smallvec![color]))
			.unzip();
		Self {
			points,
			colors,
			..default()
		}
	}
}

impl Meshable for PlanarPolyLine {
	type Output = PlanarPolylineMeshBuilder;

	fn mesh(&self) -> Self::Output {
		// FIXME: Move this impl to `PlanarPolylineMeshBuilder::build
		fn point_to_3d(p: Vec2) -> Vec3 {
			Vec3::new(p.x, 0.0, p.y)
		}
		let mut verts = Vec::with_capacity(self.points.len() * self.cross_section.len());
		let mut indices = Vec::new(); // PERF: calculate capacity
		let mut vert_colors = if self.colors.len() > 0 {
			Vec::with_capacity(verts.len())
		} else {
			Vec::new()
		};
		let mut normals = Vec::with_capacity(verts.len());
		let mut color = Color::BLACK.to_linear().to_f32_array();
		let mut colors = self.colors.iter();
		if !self.closed {
			for (b, c) in
				(1..self.cross_section.len() as u16 - 1).zip(2..self.cross_section.len() as u16)
			{
				indices.extend([0, b, c]);
			}
		}
		for (i, point) in self.points.iter().copied().enumerate() {
			let prev_i = if i == 0 { self.points.len() - 1 } else { i - 1 };
			let prev = self.points[prev_i];
			let next_i = if i == self.points.len() - 1 { 0 } else { i + 1 };
			let next = self.points[next_i];
			let a = (point - prev).normalize();
			let b = (next - point).normalize();
			let norm = (-a.perp()).lerp(-b.perp(), 0.5);
			let norm_dot_a = norm.dot(a);
			let mut colors = colors.next().into_iter().flatten();
			for (j, cross) in self.cross_section.iter().copied().enumerate() {
				let a_3d = point_to_3d(a);
				let cross_3d = Vec3::new(0.0, cross.x, cross.y);
				// Start by rotating the cross-section to align with the previous segment.
				let vert = Quat::from_rotation_arc(Vec3::NEG_X, a_3d) * cross_3d;
				// Skew the cross-section to align with the normal of the corner.
				let vert = vert + a_3d * norm_dot_a * (cross.y / norm.dot(-a.perp()));
				// Mesh normal is the normalized vertex before adding to `point`.
				let normal = vert.normalize();
				let vert = point_to_3d(point) + vert;
				verts.push(vert);
				normals.push(normal);
				if let Some(new_color) = colors.next() {
					color = new_color.to_f32_array();
				}
				if self.colors.len() > 0 {
					vert_colors.push(color);
				}

				let next_j = if j == self.cross_section.len() - 1 {
					0
				} else {
					j + 1
				};
				let n = self.cross_section.len();
				if !self.closed && i == self.points.len() - 1 {
					if j != 0 {
						indices.extend([
							(i * n) as u16,
							((i * n) + next_j) as u16,
							((i * n) + j) as u16,
						]);
					}
				} else {
					indices.extend([
						((i * n) + j) as u16,
						((next_i * n) + j) as u16,
						((i * n) + next_j) as u16,
						((i * n) + next_j) as u16,
						((next_i * n) + j) as u16,
						((next_i * n) + next_j) as u16,
					]);
				}
			}
		}
		let mut mesh = Mesh::new(
			PrimitiveTopology::TriangleList,
			RenderAssetUsages::RENDER_WORLD,
		);
		mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, verts);
		mesh.insert_attribute(Mesh::ATTRIBUTE_NORMAL, normals);
		mesh.insert_indices(Indices::U16(indices));
		if self.colors.len() > 0 {
			mesh.insert_attribute(Mesh::ATTRIBUTE_COLOR, vert_colors);
		}
		PlanarPolylineMeshBuilder(mesh)
	}
}

impl From<PlanarPolyLine> for Mesh {
	fn from(value: PlanarPolyLine) -> Self {
		value.mesh().build()
	}
}

impl PlanarPolyLine {
	#[inline]
	pub fn rect(width: f32, height: f32, thickness: f32) -> Self {
		Self {
			points: rect_points(width, height),
			cross_section: square_points(thickness),
			..default()
		}
	}

	#[inline]
	pub fn regular(sides: u32, radius: f32, thickness: f32, clocking: f32) -> Self {
		Self {
			points: polygon_points(sides, radius, clocking),
			cross_section: square_points(thickness),
			..default()
		}
	}
}

pub struct PlanarPolylineMeshBuilder(Mesh);

impl MeshBuilder for PlanarPolylineMeshBuilder {
	fn build(&self) -> Mesh {
		// FIXME: Should move `mesh` impl here instead
		self.0.clone()
	}
}