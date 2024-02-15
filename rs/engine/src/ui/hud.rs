use std::borrow::Cow;

use bevy::prelude::*;
use bevy::utils::HashMap;
use dioxus::core::{ElementId, Mutation, Mutations};
use dioxus::prelude::*;

#[derive(Debug, Copy, Clone, Default)]
pub struct DioxusPlugin;

impl Plugin for DioxusPlugin {
	fn build(&self, app: &mut App) {
		let dom = Dom::new(HelloDom);
		app.insert_non_send_resource(dom)
			.init_resource::<ElemEntityMap>()
			.add_systems(Startup, setup)
			.add_systems(Last, update_dom);
	}
}

pub fn setup(mut cmds: Commands, mut dom: NonSendMut<Dom>, mut id_map: ResMut<ElemEntityMap>) {
	let root = cmds.spawn((ElementIdComponent(ElementId(0)),)).id();
	id_map.insert(ElementId(0), root);
	let init = dom.rebuild();
	debug_assert_eq!(init.subtree, 0);
	apply_mutations(&mut cmds, id_map.reborrow(), init);
}

pub fn apply_mutations(
	mut cmds: &mut Commands,
	mut id_map: Mut<ElemEntityMap>,
	mutations: Mutations,
) {
	if mutations.edits.len() == 0 {
		return;
	}
	dbg!(&mutations);
	let Mutations {
		subtree,
		dirty_scopes,
		templates,
		edits,
	} = mutations;
	// let templates = templates
	// 	.into_iter()
	// 	.map(|temp| (temp.name, temp))
	// 	.collect::<HashMap<_, _>>();
	let mut stack = Vec::new();
	for edit in edits {
		match edit {
			Mutation::AppendChildren { id, m } => {
				let mut cmds = cmds.entity(id_map[&id]);
				for _ in 0..m {
					cmds.add_child(stack.pop().unwrap());
				}
			}
			Mutation::AssignId { path, id } => todo!(),
			Mutation::CreatePlaceholder { .. } => todo!(),
			Mutation::CreateTextNode { .. } => todo!(),
			Mutation::HydrateText { .. } => todo!(),
			Mutation::LoadTemplate { name, index, id } => {
				let template = templates.iter().find(|temp| temp.name == name).unwrap();
				let entity = match template.roots[index] {
					TemplateNode::Element {
						tag,
						namespace,
						attrs,
						children,
					} => {
						cmds.spawn((
							ElementIdComponent(id),
							ElementTag(Cow::from(tag.to_owned())),
							// TODO: maybe insert default components for attr names?
						))
						.id()
					}
					TemplateNode::Text { text } => cmds
						.spawn((
							ElementIdComponent(id),
							TextBundle {
								text: Text::from_section(text, TextStyle::default()),
								..default()
							},
						))
						.id(),
					TemplateNode::Dynamic { id: dyn_id } => cmds
						.spawn((ElementIdComponent(id), DynamicTemplateNodeId(dyn_id)))
						.id(),
					TemplateNode::DynamicText { id: dyn_id } => cmds
						.spawn((ElementIdComponent(id), DynamicTextTemplateNodeId(dyn_id)))
						.id(),
				};
				stack.push(entity);
				id_map.insert(id, entity);
			}
			Mutation::ReplaceWith { .. } => todo!(),
			Mutation::ReplacePlaceholder { .. } => todo!(),
			Mutation::InsertAfter { .. } => todo!(),
			Mutation::InsertBefore { .. } => todo!(),
			Mutation::SetAttribute {
				name,
				value,
				id,
				ns,
			} => {}
			Mutation::SetText { .. } => todo!(),
			Mutation::NewEventListener { .. } => todo!(),
			Mutation::RemoveEventListener { .. } => todo!(),
			Mutation::Remove { id } => {
				cmds.entity(id_map[&id]).despawn();
				id_map.remove(&id);
			}
			Mutation::PushRoot { id } => stack.push(id_map[&id]),
		}
	}
}

#[derive(Component, Debug, Deref, DerefMut, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ElementIdComponent(pub ElementId);

#[derive(Resource, Default, Debug, Deref, DerefMut)]
pub struct ElemEntityMap(pub HashMap<ElementId, Entity>);

#[derive(Component, Debug, Deref, DerefMut, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DynamicTemplateNodeId(pub usize);

#[derive(Component, Debug, Deref, DerefMut, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DynamicTextTemplateNodeId(pub usize);

#[derive(Component, Debug, Deref, DerefMut, Clone, PartialEq, Eq, Hash)]
pub struct ElementTag(pub Cow<'static, str>);

pub fn update_dom(mut dom: NonSendMut<Dom>) {
	let diff = dom.0.render_immediate();
	if diff.edits.len() > 0 {
		dbg!(&diff);
	}
}

#[derive(Deref, DerefMut)]
pub struct Dom(VirtualDom);

impl Dom {
	pub fn new(tree: fn(Scope) -> Element) -> Self {
		Self(VirtualDom::new(tree))
	}
}

#[component]
pub fn HelloDom(cx: Scope) -> Element {
	cx.render(rsx! {
		label {
			text: "Hello, Bevy and Dioxus!",
		},
		label {
			text: "This is a second root.",
			label {
				text: "This is a child",
			}
		}
	})
}

pub mod dioxus_elements {
	#![allow(non_camel_case_types, non_upper_case_globals)]

	use bevy::ecs::system::EntityCommand;
	use bevy::prelude::*;

	pub type AttrDesc = (&'static str, Option<&'static str>, bool);

	pub struct label;
	impl label {
		pub const TAG_NAME: &'static str = "label";
		pub const NAME_SPACE: Option<&'static str> = None;
		pub const text: AttrDesc = ("text", None, false);
	}

	pub struct sprite;
	impl sprite {
		pub const TAG_NAME: &'static str = "sprite";
		pub const NAME_SPACE: Option<&'static str> = None;
		pub const transform: AttrDesc = ("transform", None, false);
		pub const texture: AttrDesc = ("texture", None, false);
		pub const visibility: AttrDesc = ("visibility", None, false);
		pub const color: AttrDesc = ("color", Some("sprite"), false);
		pub const flip_x: AttrDesc = ("flip_x", Some("sprite"), false);
		pub const flip_y: AttrDesc = ("flip_y", Some("sprite"), false);
		pub const custom_size: AttrDesc = ("custom_size", Some("sprite"), false);
		pub const rect: AttrDesc = ("rect", Some("sprite"), false);
		pub const anchor: AttrDesc = ("anchor", Some("sprite"), false);
	}

	pub struct pbr;
	impl pbr {
		pub const TAG_NAME: &'static str = "pbr";
		pub const NAME_SPACE: Option<&'static str> = None;
		pub const mesh: AttrDesc = ("mesh", None, false);
		fn set_mesh(mesh: Handle<Mesh>) -> impl EntityCommand {
			move |mut entity: EntityWorldMut| {
				entity.insert(mesh);
			}
		}
		pub const base_color: AttrDesc = ("base_color", Some("material"), false);
		fn set_base_color(color: Color) -> impl EntityCommand {
			move |mut entity: EntityWorldMut| {
				let handle = entity.get::<Handle<StandardMaterial>>().unwrap().clone();
				entity.world_scope(|world| {
					world
						.resource_mut::<Assets<StandardMaterial>>()
						.get_mut(handle)
						.unwrap()
						.base_color = color
				});
			}
		}
		pub const emissive: AttrDesc = ("emissive", Some("material"), false);
	}
}
