use crate::ui::widgets::WidgetShape;
use ab_glyph::{Font as _, OutlineCurve::*, ScaleFont};
use bevy::{
	prelude::*,
	utils::{CowArc, HashMap},
};
use lyon_tessellation::{
	geometry_builder::simple_builder,
	math::{Point, Vector},
	path::builder::{NoAttributes, PathBuilder},
	FillOptions, FillRule, FillTessellator, VertexBuffers,
};

#[derive(Resource, Default)]
pub struct Tessellator {
	pub fill_tessellator: FillTessellator,
}

const FILL_OPTS: FillOptions = FillOptions::DEFAULT.with_fill_rule(FillRule::NonZero);

impl Tessellator {
	pub fn tessellate(
		&mut self,
		text: &str,
		Font { font }: &Font,
		tolerance: f32,
		scale: Vec2,
	) -> (VertexBuffers<Point, u16>, Rect) {
		let mut buffers = VertexBuffers::new();
		let mut builder = simple_builder(&mut buffers).with_inverted_winding();
		let opts = FILL_OPTS.with_tolerance(tolerance);
		let mut builder = self.fill_tessellator.builder(&opts, &mut builder);
		let mut bbox = Rect::new(0.0, 0.0, 0.0, 0.0);
		let mut pos = Vector::new(0.0, 0.0);
		let scale = scale / font.units_per_em().unwrap_or(1.0);
		let (em_width, em_height) = font.outline(font.glyph_id('M')).map_or((1.0, 1.0), |ol| {
			(ol.bounds.width() * scale.x, ol.bounds.height() * scale.y)
		});
		let mut last_glyph = None;
		for (char, glyph) in text.chars().map(|c| (c, font.glyph_id(c))) {
			if let Some(last_glyph) = last_glyph {
				let kerning = font.kern_unscaled(last_glyph, glyph);
				pos.x += kerning * scale.x;
			}
			if char == ' ' {
				pos.x += em_width;
			}
			let Some(outline) = font.outline(glyph) else {
				error!(?char, ?glyph, "Failed to outline glyph");
				continue;
			};
			let bounds = Rect::new(
				pos.x + (outline.bounds.min.x * scale.x),
				// Don't include tails, bottom of bbox should be the baseline
				pos.y - em_height,
				pos.x + (outline.bounds.max.x * scale.x),
				pos.y,
			);
			bbox = bbox.union(bounds);
			let mut last = Point::new(f32::NAN, f32::NAN);
			fn maybe_close(
				builder: &mut NoAttributes<impl PathBuilder>,
				from: Point,
				last: &mut Point,
			) {
				if from != *last {
					if *last == *last {
						builder.close();
					}
					builder.begin(from);
				};
			}
			for curve in dbg!(outline.curves) {
				match curve {
					Line(from, to) => {
						let from = Point::new(from.x * scale.x, from.y * scale.y) + pos;
						maybe_close(&mut builder, from, &mut last);
						last = Point::new(to.x * scale.x, to.y * scale.y) + pos;
						builder.line_to(last);
					}
					Quad(from, ctrl, to) => {
						let from = Point::new(from.x * scale.x, from.y * scale.y) + pos;
						maybe_close(&mut builder, from, &mut last);
						last = Point::new(to.x * scale.x, to.y * scale.y) + pos;
						builder.quadratic_bezier_to(
							Point::new(ctrl.x * scale.x, ctrl.y * scale.y) + pos,
							last,
						);
					}
					Cubic(from, ctrl1, ctrl2, to) => {
						let from = Point::new(from.x * scale.x, from.y * scale.y) + pos;
						maybe_close(&mut builder, from, &mut last);
						last = Point::new(to.x * scale.x, to.y * scale.y) + pos;
						builder.cubic_bezier_to(
							Point::new(ctrl1.x * scale.x, ctrl1.y * scale.y) + pos,
							Point::new(ctrl2.x * scale.x, ctrl2.y * scale.y) + pos,
							last,
						);
					}
				};
			}
			pos.x += font.h_advance_unscaled(glyph) * scale.x;
			pos.y += font.v_advance_unscaled(glyph) * scale.y;
			builder.close();
		}
		if let Err(e) = builder.build() {
			error!("{e}");
		}
		(buffers, bbox)
	}
}

#[derive(Resource, Clone, Debug)]
pub struct UiFonts {
	pub mono: Handle<Font>,
}

#[derive(Resource, Default, Clone, Debug, Deref, DerefMut)]
pub struct TextMeshCache(
	pub  HashMap<
		(CowArc<'static, str>, [u32; 16], Handle<Font>, bool),
		Option<(Handle<Mesh>, WidgetShape)>,
	>,
);