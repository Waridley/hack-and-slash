use bevy::prelude::{Commands, Component, Deref, DerefMut, Entity, EntityCommands, EntityWorldMut, Query, Ref, Res, Visibility, With};
use std::ops::ControlFlow;
use atomicow::CowArc;
use std::sync::Arc;
use smallvec::{smallvec, SmallVec};
use bevy::asset::{Asset, AssetId, Assets};
use bevy::log::{error, trace, warn};
use std::time::Duration;
use bevy::hierarchy::{Children, HierarchyQueryExt, Parent};
use leafwing_input_manager::action_state::{ActionKindData, ActionState};
use bevy::render::view::RenderLayers;
use bevy::color::{Color, LinearRgba};
use bevy::pbr::MeshMaterial3d;
use crate::ui::{MenuStack, UiAction, UiMat, GLOBAL_UI_RENDER_LAYERS};
use crate::ui::widgets::borders::Border;
use crate::ui::widgets::PrevFocus;

pub type InteractHandler =
	dyn Fn(Interaction, &mut EntityCommands) -> ControlFlow<()> + Send + Sync + 'static;

#[derive(Component, Deref, DerefMut, Clone)]
pub struct InteractHandlers(pub SmallVec<[CowArc<'static, InteractHandler>; 2]>);

impl Default for InteractHandlers {
	fn default() -> Self {
		Self(smallvec![dbg_event()])
	}
}

impl From<SmallVec<[CowArc<'static, InteractHandler>; 2]>> for InteractHandlers {
	fn from(value: SmallVec<[CowArc<'static, InteractHandler>; 2]>) -> Self {
		Self(value)
	}
}

pub fn dbg_event() -> CowArc<'static, InteractHandler> {
	CowArc::Static(&|ev, cmds| {
		let id = cmds.id();
		trace!(?id, ?ev);
		ControlFlow::Continue(())
	})
}

pub fn on_ok(
	handler: impl Fn(&mut EntityCommands) -> ControlFlow<()> + Send + Sync + 'static,
) -> CowArc<'static, InteractHandler> {
	on_action(UiAction::Ok, handler)
}

pub fn on_back(
	handler: impl Fn(&mut EntityCommands) -> ControlFlow<()> + Send + Sync + 'static,
) -> CowArc<'static, InteractHandler> {
	on_action(UiAction::Back, handler)
}

pub fn on_action(
	action: UiAction,
	handler: impl Fn(&mut EntityCommands) -> ControlFlow<()> + Send + Sync + 'static,
) -> CowArc<'static, InteractHandler> {
	CowArc::Owned(Arc::new(move |ev, cmds| {
		if ev
			== (Interaction {
				source: InteractionSource::Action(action),
				kind: InteractionKind::Begin,
			}) {
			handler(cmds)
		} else {
			ControlFlow::Continue(())
		}
	}))
}

pub fn on_focus(
	acquire: impl Fn(&mut EntityCommands) -> ControlFlow<()> + Send + Sync + 'static,
	release: impl Fn(&mut EntityCommands) -> ControlFlow<()> + Send + Sync + 'static,
) -> CowArc<'static, InteractHandler> {
	CowArc::Owned(Arc::new(move |ev, cmds| {
		if ev.source == InteractionSource::Focus {
			match ev.kind {
				InteractionKind::Begin => acquire(cmds),
				InteractionKind::Release => release(cmds),
				InteractionKind::Hold(_) => ControlFlow::Continue(()),
			}
		} else {
			ControlFlow::Continue(())
		}
	}))
}

pub fn focus_state_colors(unfocused: Color, focused: Color) -> CowArc<'static, InteractHandler> {
	focus_with_asset::<UiMat, MeshMaterial3d<UiMat>>(
		|mat| mat.0.id(),
		move |mat| mat.base.base.base_color = focused,
		move |mat| mat.base.base.base_color = unfocused,
	)
}

pub fn focus_state_emissive(
	unfocused: LinearRgba,
	focused: LinearRgba,
) -> CowArc<'static, InteractHandler> {
	focus_with_asset::<UiMat, MeshMaterial3d<UiMat>>(
		|mat| mat.0.id(),
		move |mat| mat.base.base.emissive = focused,
		move |mat| mat.base.base.emissive = unfocused,
	)
}

pub fn focus_with_asset<A: Asset, C: Component>(
	handle_getter: impl Fn(&C) -> AssetId<A> + Send + Sync + 'static,
	acquire: impl Fn(&mut A) + Send + Sync + 'static,
	release: impl Fn(&mut A) + Send + Sync + 'static,
) -> CowArc<'static, InteractHandler> {
	let handle_getter: Arc<dyn Fn(&C) -> AssetId<A> + Send + Sync + 'static> =
		Arc::new(handle_getter);
	let acquire: Arc<dyn Fn(&mut A) + Send + Sync + 'static> = Arc::new(acquire);
	let release: Arc<dyn Fn(&mut A) + Send + Sync + 'static> = Arc::new(release);
	CowArc::Owned(Arc::new(move |ev, cmds| {
		if ev.source == InteractionSource::Focus {
			let handler = match ev.kind {
				InteractionKind::Begin => acquire.clone(),
				InteractionKind::Release => release.clone(),
				InteractionKind::Hold(_) => return ControlFlow::Continue(()),
			};
			let handle_getter = handle_getter.clone();
			cmds.queue(move |mut entity: EntityWorldMut| {
				let Some(handle_component) = entity.get::<C>() else {
					let id = entity.id();
					error!(
						?id,
						"can't handle focus -- entity is missing {}",
						std::any::type_name::<C>(),
					);
					return;
				};
				let handle = handle_getter(handle_component);
				entity.world_scope(|world| {
					let mut mats = world.resource_mut::<Assets<A>>();
					let Some(mat) = mats.get_mut(handle) else {
						error!(?handle, "can't handle focus -- missing asset for id");
						return;
					};
					handler(mat);
				});
			});
			ControlFlow::Continue(())
		} else {
			ControlFlow::Continue(())
		}
	}))
}

pub fn focus_toggle_border() -> CowArc<'static, InteractHandler> {
	CowArc::Static(&|ev, cmds| {
		if ev.source == InteractionSource::Focus {
			let new_vis = match ev.kind {
				InteractionKind::Begin => Visibility::Inherited,
				InteractionKind::Release => Visibility::Hidden,
				InteractionKind::Hold(_) => return ControlFlow::Continue(()),
			};

			cmds.queue(move |mut world: EntityWorldMut| {
				let Some(children) = world.get::<Children>() else {
					warn!("no border to show focus: no children");
					return;
				};
				let children = children
					.into_iter()
					.copied()
					.collect::<SmallVec<[Entity; 8]>>();
				world.world_scope(|world| {
					let mut q = world.query_filtered::<&mut Visibility, With<Border>>();
					let mut found = false;
					for child in children {
						if let Ok(mut vis) = q.get_mut(world, child) {
							found = true;
							*vis = new_vis;
						}
					}
					if !found {
						warn!("no border to show focus: no child matches `Query<&mut Visibility, With<Border>>`");
					}
				});
			});
		}
		ControlFlow::Continue(())
	})
}

impl InteractHandlers {
	pub fn on_ok(
		handler: impl Fn(&mut EntityCommands) -> ControlFlow<()> + Send + Sync + 'static,
	) -> Self {
		Self::on_action(UiAction::Ok, handler)
	}

	pub fn on_back(
		handler: impl Fn(&mut EntityCommands) -> ControlFlow<()> + Send + Sync + 'static,
	) -> Self {
		Self::on_action(UiAction::Back, handler)
	}

	pub fn on_action(
		action: UiAction,
		handler: impl Fn(&mut EntityCommands) -> ControlFlow<()> + Send + Sync + 'static,
	) -> Self {
		Self(smallvec![dbg_event(), on_action(action, handler)])
	}

	pub fn handle(&self, event: Interaction, cmds: &mut EntityCommands) -> ControlFlow<()> {
		self.iter().try_for_each(|handler| handler(event, cmds))
	}

	pub fn extend(&mut self, handlers: impl IntoIterator<Item = CowArc<'static, InteractHandler>>) {
		self.0.extend(handlers)
	}

	pub fn and(
		mut self,
		handlers: impl IntoIterator<Item = CowArc<'static, InteractHandler>>,
	) -> Self {
		self.extend(handlers);
		self
	}

	pub fn system(
		mut cmds: Commands,
		q: Query<&InteractHandlers>,
		parents: Query<&Parent>,
		global_state: Res<ActionState<UiAction>>,
		states: Query<(&ActionState<UiAction>, &RenderLayers)>,
		mut stacks: Query<(Ref<MenuStack>, &mut PrevFocus, &RenderLayers)>,
	) {
		for (state, layers) in
			std::iter::once((&*global_state, &GLOBAL_UI_RENDER_LAYERS)).chain(&states)
		{
			let Some((stack, mut prev_focus, _)) = stacks
				.iter_mut()
				.find(|(_, _, cam_layers)| **cam_layers == *layers)
			else {
				error!("no camera for {layers:?}");
				continue;
			};
			let Some(focus) = stack.last().map(|menu| menu.focus) else {
				continue;
			};
			for action in state.get_just_pressed() {
				let ev = Interaction {
					source: InteractionSource::Action(action),
					kind: InteractionKind::Begin,
				};
				let _ = propagate_interaction(&mut cmds, focus, ev, &q, &parents);
			}
			for action in state.get_pressed() {
				let data = state
					.action_data(&action)
					.expect("action is pressed ∴ ActionData exists");
				match &data.kind_data {
					ActionKindData::Button(data) => {
						let ev = Interaction {
							source: InteractionSource::Action(action),
							kind: InteractionKind::Hold(data.timing.current_duration),
						};
						let _ = propagate_interaction(&mut cmds, focus, ev, &q, &parents);
					}
					data => warn!(?data, "Only Button timing is supported"),
				}
			}
			for action in state.get_just_released() {
				let ev = Interaction {
					source: InteractionSource::Action(action),
					kind: InteractionKind::Release,
				};
				let _ = propagate_interaction(&mut cmds, focus, ev, &q, &parents);
			}
			if focus != **prev_focus {
				let release = Interaction {
					source: InteractionSource::Focus,
					kind: InteractionKind::Release,
				};
				let _ = propagate_interaction(&mut cmds, **prev_focus, release, &q, &parents);

				let begin = Interaction {
					source: InteractionSource::Focus,
					kind: InteractionKind::Begin,
				};
				let _ = propagate_interaction(&mut cmds, focus, begin, &q, &parents);
				**prev_focus = focus;
			}
		}
	}
}

// TODO: Replace with bevy_picking and/or triggers
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

impl FromIterator<CowArc<'static, InteractHandler>> for InteractHandlers {
	fn from_iter<T: IntoIterator<Item = CowArc<'static, InteractHandler>>>(iter: T) -> Self {
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