use super::icons::Icon;
use crate::{
	node_3d, node_3d_defaults,
	ui::{
		a11y::AKNode,
		text::UiFonts,
		widgets::{Text3d, WidgetShape, UNLIT_MATERIAL_ID},
		UiMat,
	},
};
use bevy::{ecs::system::EntityCommands, prelude::*, render::view::RenderLayers};
use bevy_rapier3d::parry::{math::Isometry, shape::SharedShape};
use bevy_svg::{
	prelude::{Origin, Svg},
	SvgSettings,
};

#[derive(Component, Debug, Clone)]
pub struct InputIcon {
	pub icon: Icon,
	pub size: Vec3,
	pub flat: bool,
	pub text_entity: Entity,
	pub isometry: Isometry<f32>,
}

impl Default for InputIcon {
	fn default() -> Self {
		Self {
			icon: default(),
			size: Vec3::ONE,
			flat: true,
			text_entity: Entity::PLACEHOLDER,
			isometry: default(),
		}
	}
}

node_3d! { InputIconBundle<M: Material = UiMat> {
	input_icon: InputIcon,
	font: Handle<Font>,
	material: Handle<M>,
}}

impl<M: Material> Default for InputIconBundle<M> {
	fn default() -> Self {
		node_3d_defaults! {
			input_icon: default(),
			font: default(),
			material: Handle::weak_from_u128(UNLIT_MATERIAL_ID),
		}
	}
}

impl InputIcon {
	pub fn sync<M: Material>(
		mut cmds: Commands,
		mut q: Query<(Entity, &mut InputIcon, &mut AKNode, &Handle<M>), Changed<InputIcon>>,
		asset_server: Res<AssetServer>,
		fonts: Res<UiFonts>,
	) {
		for (id, mut this, mut ak_node, mat) in &mut q {
			let Self {
				icon: Icon {
					ref image,
					ref text,
				},
				size,
				flat,
				text_entity,
				isometry,
			} = *this;
			cmds.get_entity(text_entity)
				.map(EntityCommands::despawn_recursive);
			let mut cmds = cmds.entity(id);
			let svg = asset_server.load_with_settings::<Svg, SvgSettings>(image, move |settings| {
				settings.transform.rotation *= Quat::from_rotation_arc(Vec3::Y, Vec3::Z);
				settings.origin = Origin::Center;
				settings.size = Some(size.xz());
				settings.depth = (!flat).then_some(size.y);
			});
			// `bevy_svg` doesn't insert a mesh if a handle isn't already present. PR?
			cmds.insert((svg, Handle::<Mesh>::default()));
			let half_size = size * 0.5;
			cmds.insert(WidgetShape {
				shape: SharedShape::cuboid(half_size.x, half_size.y, half_size.z),
				isometry,
			});
			text.clone().map(|text| {
				// FIXME: Add descriptions for icons without text
				ak_node.set_description(&*text);
				cmds.with_children(|cmds| {
					// Avoid re-triggering sync every frame.
					let this = this.bypass_change_detection();
					this.text_entity = cmds
						.spawn(crate::ui::widgets::Text3dBundle {
							text_3d: Text3d {
								text: text.to_string().into(),
								flat,
								vertex_scale: Vec3::new(size.x * 0.5, size.y * 0.5, size.z),
								..default()
							},
							font: fonts.mono.clone(),
							material: mat.clone(),
							..default()
						})
						.id();
				});
			});
		}
	}
}
