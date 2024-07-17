use super::widgets::WidgetShape;
use crate::{
	todo_err, todo_warn,
	ui::{GlobalUi, MenuStack, UiAction, UiCam, GLOBAL_UI_RENDER_LAYERS},
};
use bevy::{
	input::mouse::MouseMotion,
	prelude::*,
	render::{
		camera::{RenderTarget, Viewport},
		view::{Layer, RenderLayers},
	},
	window::{PrimaryWindow, WindowRef},
};
use bevy_rapier3d::parry::{
	math::{Isometry, Translation, Vector},
	query::Ray,
};
use leafwing_input_manager::prelude::InputMap;
use std::cmp::Ordering;

pub fn mouse_picks_focus(
	q: Query<(&GlobalTransform, &WidgetShape)>,
	mut menu_stacks: Query<(&mut MenuStack, &RenderLayers)>,
	descendant_q: Query<&Children>,
	ancestor_q: Query<&Parent>,
	mut events: EventReader<CursorMoved>,
	mouse_layers: Res<MouseLayers>,
	windows: Query<(&Window, Has<PrimaryWindow>)>,
	cams: Query<(&Camera, &GlobalTransform, &RenderLayers), With<UiCam>>,
) {
	for ev in events.read() {
		for (cam, cam_xform, cam_layers) in cams.iter() {
			if !mouse_layers.intersects(cam_layers) {
				continue;
			}

			// Check that this camera is rendering to the window that fired this event
			match &cam.target {
				RenderTarget::Window(WindowRef::Primary)
					if windows
						.get(ev.window)
						.expect("event requires window to exist")
						.1 => {}
				RenderTarget::Window(WindowRef::Entity(id)) if ev.window == *id => {}
				other => {
					warn!(?cam, "not handling mouse focus for {other:?}");
					continue;
				}
			}

			let Some(vp) = cam.logical_viewport_rect() else {
				// might not be computed yet this frame
				debug!(?cam, "can't get logical_viewport_rect");
				continue;
			};
			let pos = ev.position - vp.min;
			let Some(ray) = cam.viewport_to_world(cam_xform, pos) else {
				continue;
			};
			let ray = Ray::new(ray.origin.into(), (*ray.direction).into());

			// TODO: This is awkward, might want to have `UiCam` store the entity for its `MenuStack`
			let Some((mut stack, _)) = menu_stacks
				.iter_mut()
				.find(|(_, layers)| *cam_layers == **layers)
			else {
				continue;
			};

			if stack.is_empty() {
				continue;
			}
			// A `try_map_unchanged` method would be nice
			let mut menu = stack
				.map_unchanged(|stack| stack.last_mut().expect("just ensured stack is not empty"));

			let mut all_intersections = descendant_q
				.iter_descendants(menu.root)
				.chain(std::iter::once(menu.root))
				.chain(ancestor_q.iter_ancestors(menu.root))
				.filter_map(|entity| q.get(entity).ok().zip(Some(entity)))
				.filter_map(|((xform, shape), entity)| {
					let xform = xform.compute_transform();
					let iso = Isometry::from_parts(
						(Vec3::from(shape.isometry.translation.vector) + xform.translation).into(),
						(Quat::from(shape.isometry.rotation) * xform.rotation).into(),
					);
					shape
						.shape
						.0
						.cast_ray(&iso, &ray, 1000.0, true)
						.map(|toi| (entity, toi))
				})
				.collect::<Vec<_>>();
			all_intersections
				.iter()
				.filter(|(id, toi)| {
					descendant_q
						// If no descendants exist, this will automatically be a leaf.
						.iter_descendants(*id)
						// But if the entity has descendants, it could still be a leaf for the purpose
						// of mouse picking if no children are intersected by the ray.
						.filter(|descendant|
						// If any descendant is intersected by the ray, do not count this as a leaf
						all_intersections.iter().any(|(intersected, _)| *intersected == *descendant))
						// If no descendant is intersected by the ray, this is a leaf.
						.next()
						.is_none()
				})
				// Only check the closest leaf
				.min_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap_or(Ordering::Equal))
				.map(|(id, _)| menu.focus = *id);
		}
	}
}

/// What UI layers the mouse interacts with.
#[derive(Resource, Debug, Deref, DerefMut, Reflect)]
#[reflect(Resource)]
pub struct MouseLayers(pub RenderLayers);

impl Default for MouseLayers {
	fn default() -> Self {
		Self(GLOBAL_UI_RENDER_LAYERS)
	}
}
