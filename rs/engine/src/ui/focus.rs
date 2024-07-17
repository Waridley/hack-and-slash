use crate::{
	todo_warn,
	ui::{
		widgets::{InteractHandler, InteractionKind, InteractionSource, WidgetGizmos, WidgetShape},
		GlobalUi, MenuRef, MenuStack, UiAction, UiCam, GLOBAL_UI_RENDER_LAYERS,
	},
};
use bevy::{
	a11y::Focus,
	ecs::{identifier::error::IdentifierError, query::QueryEntityError},
	prelude::*,
	render::view::{Layer, RenderLayers},
	utils::CowArc,
};
use leafwing_input_manager::prelude::ActionState;
use serde::{Deserialize, Serialize};
use std::{
	f32::consts::{FRAC_PI_2, FRAC_PI_3, FRAC_PI_4, FRAC_PI_6, FRAC_PI_8, PI},
	fmt::Formatter,
	num::ParseIntError,
	ops::ControlFlow,
	str::{FromStr, Split},
	sync::Arc,
};

/// Tells [handle_focus_actions] how to find the next entity to focus.
///
/// Can be parsed from a string by means of the [FromStr] implementation.
/// See [FocusTarget::Path] for a complete example.
// TODO: Impl [De]Serialize in terms of `FromStr`
#[derive(Debug, Clone, PartialEq, Eq, Hash, Reflect, Serialize, Deserialize)]
#[reflect(no_field_bounds, Serialize, Deserialize)]
pub enum FocusTarget {
	/// Finds the sibling relative to this entity in the parent's `Children` list.
	/// Wraps around the ends of the list.
	///
	/// Represented in a string prefixed by `+` or `-`.
	Sibling(isize),
	/// Focuses the immediate parent of this entity.
	///
	/// Represented by the string `".."`.
	ToParent,
	/// Focuses the root of the current menu.
	///
	/// Represented by an empty string (so a path like `"/..." starts at the root of the menu).
	MenuRoot,
	/// Focuses the nth child of this entity.
	///
	/// Represented in a string prefixed by `#`.
	ChildN(usize),
	/// Finds the first immediate child of this entity with the given name.
	///
	/// Represented in a string surrounded by quotes, like `"\"SomeName\""`.
	FindChild(Name),
	/// Finds the first descendant of this entity with the given name.
	///
	/// Represented in a string surrounded by square brackets, like `"[SomeName]"`.
	FindDescendant(Name),
	/// Jumps directly to the entity with the given ID.
	///
	/// Not often used in a string, but it can be represented by the bits as a `u64` prefixed by `$`,
	/// e.g. `"$42"`.
	Entity(Entity),
	/// A path consisting of multiple `FocusTarget` segments.
	///
	/// Represented in a string by each segment separated by `/`.
	/// Example:
	/// ```
	/// # use bevy::prelude::Name;
	/// use sond_has_engine::ui::focus::FocusTarget::{self, *};
	/// assert_eq!(
	///     r#"/#3/"#AChildsName"/+1/[ADescendantsName]/.."#
	///         .parse::<FocusTarget>()
	///         .unwrap(),
	///     Path(vec![
	///         MenuRoot,
	///         ChildN(3),
	///         FindChild(Name::new("AChildsName")),
	///         Sibling(1),
	///         FindDescendant(Name::new("ADescendantsName")),
	///         ToParent,
	///     ]),
	/// )
	/// ```
	Path(Vec<Self>),
}

impl FromStr for FocusTarget {
	type Err = TargetParseError;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		let entries = s
			.split('/')
			.map(|seg| {
				Ok(if seg == "" {
					Self::MenuRoot
				} else if seg == ".." {
					Self::ToParent
				} else {
					let Some((prefix, rest)) = seg.split_at_checked(1) else {
						return Err(TargetParseError::InvalidSegment(seg.to_owned()));
					};
					let prefix = prefix.chars().next().expect(
						"`split_at_checked(1)` should return a single character on the left",
					);
					match prefix {
						'#' => Self::ChildN(rest.parse()?),
						'+' => Self::Sibling(rest.parse()?),
						'-' => Self::Sibling(-rest.parse()?),
						'$' => Self::Entity(Entity::try_from_bits(rest.parse()?)?),
						'"' => {
							let Some(name) = rest.strip_suffix('"') else {
								return Err(TargetParseError::InvalidSegment(seg.to_owned()));
							};
							Self::FindChild(Name::new(name.to_owned()))
						}
						'[' => {
							let Some(name) = rest.strip_suffix(']') else {
								return Err(TargetParseError::InvalidSegment(seg.to_owned()));
							};
							Self::FindDescendant(Name::new(name.to_owned()))
						}
						other => return Err(TargetParseError::UnknownPrefix(other)),
					}
				})
			})
			.collect::<Result<Vec<_>, Self::Err>>()?;
		Ok(match <[Self; 1]>::try_from(entries) {
			Ok([single]) => single,
			Err(path) => Self::Path(path),
		})
	}
}

#[derive(Debug, Clone)]
pub enum TargetParseError {
	ParseInt(ParseIntError),
	ParseEntity(IdentifierError),
	InvalidSegment(String),
	UnknownPrefix(char),
}

impl From<ParseIntError> for TargetParseError {
	fn from(value: ParseIntError) -> Self {
		Self::ParseInt(value)
	}
}

impl From<IdentifierError> for TargetParseError {
	fn from(value: IdentifierError) -> Self {
		Self::ParseEntity(value)
	}
}

impl std::fmt::Display for TargetParseError {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			TargetParseError::ParseInt(e) => e.fmt(f),
			TargetParseError::ParseEntity(e) => e.fmt(f),
			TargetParseError::InvalidSegment(s) => write!(f, "invalid segment: {s}"),
			TargetParseError::UnknownPrefix(c) => write!(f, "unknown prefix: {c}"),
		}
	}
}

impl FocusTarget {
	pub fn resolve(
		&self,
		from: Entity,
		parent_query: &Query<&Parent>,
		children_query: &Query<&Children>,
		names: &Query<&Name>,
		menu: &MenuRef,
	) -> Option<Entity> {
		match self {
			Self::Sibling(relative) => parent_query
				.get(from)
				.ok()
				.map(Parent::get)
				.and_then(|parent| children_query.get(parent).ok())
				.and_then(|children| {
					let len = children.len();
					let i = (children
						.iter()
						.position(|child| *child == from)
						.unwrap()
						.checked_add_signed(*relative)
						.unwrap_or(len - 1))
						% len;
					children.get(i).copied()
				}),
			Self::ToParent => parent_query.get(from).ok().map(Parent::get),
			Self::MenuRoot => Some(menu.root),
			Self::ChildN(i) => children_query
				.get(from)
				.ok()
				.and_then(|children| children.get(*i).copied()),
			Self::FindChild(name) => children_query.get(from).ok().and_then(|children| {
				children.iter().copied().find(|child| {
					let Some(child_name) = names.get(*child).ok() else {
						return false;
					};
					*child_name == *name
				})
			}),
			Self::FindDescendant(name) => children_query.iter_descendants(from).find(|child| {
				let Some(child_name) = names.get(*child).ok() else {
					return false;
				};
				*child_name == *name
			}),
			Self::Entity(id) => Some(*id),
			Self::Path(path) => {
				let mut from = from;
				for item in path {
					from = item.resolve(from, parent_query, children_query, names, menu)?;
				}
				Some(from)
			}
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

#[derive(Component, Debug, Clone, Serialize, Deserialize)]
pub struct AdjacentWidgets {
	pub prev: Option<FocusTarget>,
	pub next: Option<FocusTarget>,
	pub directions: Vec<(Wedge2d, FocusTarget)>,
}

impl Default for AdjacentWidgets {
	fn default() -> Self {
		// Bubbling up to the parent seems the safest if a child is not given explicit focus targets.
		// The topmost entity in a menu tree is the easiest to remember to check that the focus
		// navigation behavior is robust and intuitive.
		Self::all(FocusTarget::ToParent)
	}
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
		Self::horizontal(FocusTarget::Sibling(-1), FocusTarget::Sibling(1))
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
		Self::vertical(FocusTarget::Sibling(-1), FocusTarget::Sibling(1))
	}

	pub fn all(target: FocusTarget) -> Self {
		Self {
			prev: Some(target.clone()),
			next: Some(target.clone()),
			directions: vec![(Wedge2d::circle(), target)],
		}
	}
}

pub fn handle_focus_actions(
	q: Query<(&AdjacentWidgets, Option<&Parent>, &RenderLayers)>,
	parents_q: Query<&Parent>,
	children_q: Query<&Children>,
	names: Query<&Name>,
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
					.resolve(menu.focus, &parents_q, &children_q, &names, &*menu)
					.unwrap_or(menu.focus);
				focus.0 = Some(menu.focus);
			}
		}
		if state.just_pressed(&UiAction::FocusPrev) {
			if let Some(prev) = &curr.0.prev {
				menu.focus = prev
					.resolve(menu.focus, &parents_q, &children_q, &names, &*menu)
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
							.resolve(menu.focus, &parents_q, &children_q, &names, &*menu)
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

#[cfg(feature = "debugging")]
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

#[cfg(feature = "debugging")]
#[derive(Default, Debug, GizmoConfigGroup, Reflect)]
pub struct FocusGizmos<const LAYER: Layer>;
