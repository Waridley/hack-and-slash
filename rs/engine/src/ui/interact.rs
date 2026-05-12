use crate::ui::{
	widgets::{borders::Border, PrevFocus},
	MenuStack, UiAction, UiMat, GLOBAL_UI_RENDER_LAYERS,
};
use atomicow::CowArc;
use bevy::{
	asset::{Asset, AssetId, Assets},
	color::{Color, LinearRgba},
	log::{error, trace, warn},
	pbr::MeshMaterial3d,
	prelude::*,
	render::view::RenderLayers,
};
use leafwing_input_manager::action_state::{ActionKindData, ActionState};
use smallvec::{smallvec, SmallVec};
use std::{ops::ControlFlow, sync::Arc, time::Duration};

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

pub fn dbg_event_observer(trigger: Trigger<Interaction>) {
	trace!(event=?trigger.event(), entity=?trigger.observer());
}

pub fn bridge_to_handlers_observer(
	trigger: Trigger<Interaction>,
	handlers_q: Query<&InteractHandlers>,
	mut cmds: Commands,
) {
	let entity = trigger.target();
	let Ok(handlers) = handlers_q.get(entity) else {
		return;
	};
	let mut entity_cmds = cmds.entity(entity);
	let _ = handlers.handle(*trigger.event(), &mut entity_cmds);
}

pub fn observe_interact_handlers(mut cmds: Commands, q: Query<Entity, Added<InteractHandlers>>) {
	for id in &q {
		cmds.entity(id).observe(bridge_to_handlers_observer);
	}
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

pub fn focus_toggle_border_observer(
	trigger: Trigger<Interaction>,
	children_q: Query<&Children>,
	mut vis_q: Query<&mut Visibility, With<Border>>,
) {
	if trigger.event().source != InteractionSource::Focus {
		return;
	}
	let new_vis = match trigger.event().kind {
		InteractionKind::Begin => Visibility::Inherited,
		InteractionKind::Release => Visibility::Hidden,
		InteractionKind::Hold(_) => return,
	};
	let entity = trigger.target();
	let Ok(children) = children_q.get(entity) else {
		warn!("no border to show focus: no children");
		return;
	};
	let mut found = false;
	for &child in children.into_iter() {
		if let Ok(mut vis) = vis_q.get_mut(child) {
			found = true;
			*vis = new_vis;
		}
	}
	if !found {
		warn!("no border to show focus: no child matches `Query<&mut Visibility, With<Border>>`");
	}
}

#[derive(Component, Debug, Reflect, Clone)]
#[reflect(Component)]
pub struct FocusStateColors {
	pub unfocused: Color,
	pub focused: Color,
}

// TODO: migrate to pure observer pattern once Bevy supports closures as IntoObserverSystem
// (see https://github.com/bevyengine/bevy/issues — closures don't implement System trait)
// Currently this uses a component (FocusStateColors) + standalone observer function as a workaround.
pub fn focus_state_colors_observer_setup(
	cmds: &mut EntityCommands,
	unfocused: Color,
	focused: Color,
) {
	cmds.insert(FocusStateColors { unfocused, focused });
	cmds.observe(focus_state_colors_observer);
}

pub fn focus_state_colors_observer(
	trigger: Trigger<Interaction>,
	focus_colors: Query<&FocusStateColors>,
	mut mats: ResMut<Assets<UiMat>>,
	mat_q: Query<&MeshMaterial3d<UiMat>>,
) {
	if trigger.event().source != InteractionSource::Focus {
		return;
	}
	let entity = trigger.target();
	let Ok(focus) = focus_colors.get(entity) else {
		return;
	};
	let color = match trigger.event().kind {
		InteractionKind::Begin => focus.focused,
		InteractionKind::Release => focus.unfocused,
		InteractionKind::Hold(_) => return,
	};
	let Ok(handle) = mat_q.get(entity) else {
		return;
	};
	let Some(mat) = mats.get_mut(handle.id()) else {
		return;
	};
	mat.base.base.base_color = color;
}

#[derive(Component, Debug, Reflect, Clone)]
#[reflect(Component)]
pub struct FocusStateEmissive {
	pub unfocused: LinearRgba,
	pub focused: LinearRgba,
}

// TODO: migrate to pure observer pattern once Bevy supports closures as IntoObserverSystem
pub fn focus_state_emissive_observer_setup(
	cmds: &mut EntityCommands,
	unfocused: LinearRgba,
	focused: LinearRgba,
) {
	cmds.insert(FocusStateEmissive { unfocused, focused });
	cmds.observe(focus_state_emissive_observer);
}

pub fn focus_state_emissive_observer(
	trigger: Trigger<Interaction>,
	focus_emissive: Query<&FocusStateEmissive>,
	mut mats: ResMut<Assets<UiMat>>,
	mat_q: Query<&MeshMaterial3d<UiMat>>,
) {
	if trigger.event().source != InteractionSource::Focus {
		return;
	}
	let entity = trigger.target();
	let Ok(focus) = focus_emissive.get(entity) else {
		return;
	};
	let emissive = match trigger.event().kind {
		InteractionKind::Begin => focus.focused,
		InteractionKind::Release => focus.unfocused,
		InteractionKind::Hold(_) => return,
	};
	let Ok(handle) = mat_q.get(entity) else {
		return;
	};
	let Some(mat) = mats.get_mut(handle.id()) else {
		return;
	};
	mat.base.base.emissive = emissive;
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
				cmds.trigger_targets(
					Interaction {
						source: InteractionSource::Action(action),
						kind: InteractionKind::Begin,
					},
					focus,
				);
			}
			for action in state.get_pressed() {
				let data = state
					.action_data(&action)
					.expect("action is pressed ∴ ActionData exists");
				match &data.kind_data {
					ActionKindData::Button(data) => {
						cmds.trigger_targets(
							Interaction {
								source: InteractionSource::Action(action),
								kind: InteractionKind::Hold(data.timing.current_duration),
							},
							focus,
						);
					}
					data => warn!(?data, "Only Button timing is supported"),
				}
			}
			for action in state.get_just_released() {
				cmds.trigger_targets(
					Interaction {
						source: InteractionSource::Action(action),
						kind: InteractionKind::Release,
					},
					focus,
				);
			}
			if focus != **prev_focus {
				cmds.trigger_targets(
					Interaction {
						source: InteractionSource::Focus,
						kind: InteractionKind::Release,
					},
					**prev_focus,
				);
				cmds.trigger_targets(
					Interaction {
						source: InteractionSource::Focus,
						kind: InteractionKind::Begin,
					},
					focus,
				);
				**prev_focus = focus;
			}
		}
	}
}

impl FromIterator<CowArc<'static, InteractHandler>> for InteractHandlers {
	fn from_iter<T: IntoIterator<Item = CowArc<'static, InteractHandler>>>(iter: T) -> Self {
		Self(iter.into_iter().collect())
	}
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum InteractionSource {
	Action(UiAction),
	Focus,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum InteractionKind {
	Begin,
	Hold(Duration),
	Release,
}

#[derive(Component, Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Interaction {
	pub source: InteractionSource,
	pub kind: InteractionKind,
}

impl Event for Interaction {
	type Traversal = &'static ChildOf;
	const AUTO_PROPAGATE: bool = true;
}
