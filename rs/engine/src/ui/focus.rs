use crate::{
	todo_warn,
	ui::{
		widgets::{WidgetGizmos, WidgetShape},
		GlobalUi, MenuStack, UiAction, UiCam, GLOBAL_UI_RENDER_LAYERS,
	},
};
use bevy::{
	a11y::Focus,
	prelude::*,
	render::view::{Layer, RenderLayers},
};
use leafwing_input_manager::prelude::ActionState;
use serde::{Deserialize, Serialize};
use std::f32::consts::{FRAC_PI_2, FRAC_PI_3, FRAC_PI_4, FRAC_PI_6, FRAC_PI_8, PI};

#[derive(Debug, Clone, Reflect, Serialize, Deserialize)]
#[reflect(Serialize, Deserialize)]
pub enum FocusTarget {
	NextSibling,
	PrevSibling,
	Sibling(usize),
	Parent,
	Child(usize),
	// TODO: EntityPath doesn't implmenet [De]Serialize
	// Path(EntityPath),
	Entity(Entity),
}

impl FocusTarget {
	pub fn resolve(
		&self,
		from: Entity,
		parent: Option<&Parent>,
		children_query: &Query<&Children>,
	) -> Option<Entity> {
		match self {
			FocusTarget::Sibling(i) => parent
				.map(Parent::get)
				.and_then(|parent| children_query.get(parent).ok())
				.and_then(|children| children.get(*i).copied()),
			FocusTarget::NextSibling => parent
				.map(Parent::get)
				.and_then(|parent| children_query.get(parent).ok())
				.and_then(|children| {
					children
						.get(
							(children.iter().position(|child| *child == from).unwrap() + 1)
								% children.len(),
						)
						.copied()
				}),
			FocusTarget::PrevSibling => parent
				.map(Parent::get)
				.and_then(|parent| children_query.get(parent).ok())
				.and_then(|children| {
					children
						.get(
							(children.iter().position(|child| *child == from).unwrap()
								+ children.len() - 1) % children.len(),
						)
						.copied()
				}),
			FocusTarget::Parent => parent.map(Parent::get),
			FocusTarget::Child(i) => children_query
				.get(from)
				.ok()
				.and_then(|children| children.get(*i).copied()),
			// FocusTarget::Path(_) => todo_warn!(),
			FocusTarget::Entity(id) => Some(*id),
		}
	}
}

#[derive(Copy, Clone, Debug, PartialEq, Serialize, Deserialize, Reflect)]
#[reflect(Serialize, Deserialize)]
pub struct Wedge2d {
	pub direction: Vec2,
	pub half_angle: f32,
}

impl Wedge2d {
	pub fn new(direction: Vec2, half_angle: f32) -> Self {
		Self {
			direction,
			half_angle,
		}
	}

	pub const fn half(direction: Vec2) -> Self {
		Self {
			direction,
			half_angle: FRAC_PI_2,
		}
	}

	pub const fn third(direction: Vec2) -> Self {
		Self {
			direction,
			half_angle: FRAC_PI_3,
		}
	}

	pub const fn quarter(direction: Vec2) -> Self {
		Self {
			direction,
			half_angle: FRAC_PI_4,
		}
	}

	pub const fn sixth(direction: Vec2) -> Self {
		Self {
			direction,
			half_angle: FRAC_PI_6,
		}
	}

	pub const fn eighth(direction: Vec2) -> Self {
		Self {
			direction,
			half_angle: FRAC_PI_8,
		}
	}

	pub const fn circle() -> Self {
		Self {
			direction: Vec2::X,
			half_angle: PI,
		}
	}

	pub fn contains(self, vec: Vec2) -> bool {
		self.direction.angle_between(vec).abs() - f32::EPSILON <= self.half_angle
	}
}

#[derive(Component, Debug, Default, Clone, Serialize, Deserialize)]
pub struct AdjacentWidgets {
	pub prev: Option<FocusTarget>,
	pub next: Option<FocusTarget>,
	pub directions: Vec<(Wedge2d, FocusTarget)>,
}

impl AdjacentWidgets {
	pub fn horizontal(left: FocusTarget, right: FocusTarget) -> Self {
		Self {
			prev: Some(left.clone()),
			next: Some(right.clone()),
			directions: vec![
				(Wedge2d::quarter(Vec2::X), right),
				(Wedge2d::quarter(Vec2::NEG_X), left),
			],
		}
	}

	pub fn horizontal_siblings() -> Self {
		Self::horizontal(FocusTarget::PrevSibling, FocusTarget::NextSibling)
	}

	pub fn vertical(up: FocusTarget, down: FocusTarget) -> Self {
		Self {
			prev: Some(up.clone()),
			next: Some(down.clone()),
			directions: vec![
				(Wedge2d::quarter(Vec2::Y), up),
				(Wedge2d::quarter(Vec2::NEG_Y), down),
			],
		}
	}

	pub fn vertical_siblings() -> Self {
		Self::vertical(FocusTarget::PrevSibling, FocusTarget::NextSibling)
	}

	pub fn all(target: FocusTarget) -> Self {
		Self {
			prev: Some(target.clone()),
			next: Some(target.clone()),
			directions: vec![(Wedge2d::circle(), target)],
		}
	}
}

pub fn resolve_focus(
	q: Query<(&AdjacentWidgets, Option<&Parent>, &RenderLayers)>,
	parents: Query<&Children>,
	mut stacks: Query<(&mut MenuStack, &RenderLayers)>,
	actions: Query<(&ActionState<UiAction>, &RenderLayers)>,
	glob_actions: Res<ActionState<UiAction>>,
	mut focus: ResMut<Focus>,
	mut prev_cursor: Local<Option<Wedge2d>>,
) {
	for (state, layers) in
		std::iter::once((&*glob_actions, &GLOBAL_UI_RENDER_LAYERS)).chain(actions.iter())
	{
		let Some((mut cam, _)) = stacks
			.iter_mut()
			.find(|(_, cam_layers)| **cam_layers == *layers)
		else {
			error!("`MenuStack` should exist for all `RenderLayers` for which `ActionState<UiAction>` exists");
			continue;
		};
		let Some((mut menu, mut curr)) = cam
			.last_mut()
			.and_then(|menu| q.get(menu.focus).ok().map(|curr| (menu, curr)))
		else {
			continue;
		};

		if state.just_pressed(&UiAction::FocusNext) {
			if let Some(next) = &curr.0.next {
				menu.focus = next
					.resolve(menu.focus, curr.1, &parents)
					.unwrap_or(menu.focus);
				focus.0 = Some(menu.focus);
			}
		}
		if state.just_pressed(&UiAction::FocusPrev) {
			if let Some(prev) = &curr.0.prev {
				menu.focus = prev
					.resolve(menu.focus, curr.1, &parents)
					.unwrap_or(menu.focus);
				focus.0 = Some(menu.focus);
			}
		}
		if let Some(dir) = state.clamped_axis_pair(&UiAction::MoveCursor) {
			if dir.length() > 0.5 {
				let (dir, target) = curr
					.0
					.directions
					.iter()
					.find_map(|(wedge, target)| {
						wedge.contains(dir.xy()).then_some((*wedge, target))
					})
					.unzip();
				if *prev_cursor != dir {
					if let Some(target) = target {
						menu.focus = target
							.resolve(menu.focus, curr.1, &parents)
							.unwrap_or(menu.focus);
						focus.0 = Some(menu.focus);
					}
					*prev_cursor = dir;
				}
			} else {
				*prev_cursor = None;
			}
		} else {
			*prev_cursor = None;
		}
	}
}

pub fn highlight_focus<const LAYER: Layer>(
	mut gizmos: Gizmos<FocusGizmos<LAYER>>,
	q: Query<(
		Entity,
		&GlobalTransform,
		&WidgetShape,
		&ViewVisibility,
		&RenderLayers,
	)>,
	mut stack: Query<(&mut MenuStack, &RenderLayers)>,
) {
	let Some(mut focus) = stack
		.iter_mut()
		.find(|(stack, layers)| **layers == RenderLayers::layer(LAYER))
		.and_then(|(stack, _)| {
			if stack.last().is_some() {
				Some(stack.map_unchanged(|stack| &mut stack.last_mut().unwrap().focus))
			} else {
				None
			}
		})
	else {
		return;
	};
	let focus = *focus;
	match q.get(focus) {
		Ok((id, xform, shape, vis, layers)) => {
			if !**vis {
				return;
			}
			if !gizmos.config.render_layers.intersects(layers) {
				return;
			}
			let (scale, rot, pos) = xform.to_scale_rotation_translation();
			#[cfg(feature = "debugging")]
			if (scale.x - scale.y).abs() > 0.00001 || (scale.x - scale.z).abs() > 0.00001 {
				warn!(
					?id,
					?shape,
					?scale,
					"widgets should have a uniform scale -- only drawing using `scale.x`"
				)
			}
			shape.draw_gizmo(&mut gizmos, pos, rot, scale.x, Color::SEA_GREEN);
		}
		Err(e) => {
			error!("couldn't get focused entity: {e}");
		}
	}
}

#[derive(Default, Debug, GizmoConfigGroup, Reflect)]
pub struct FocusGizmos<const LAYER: Layer>;
