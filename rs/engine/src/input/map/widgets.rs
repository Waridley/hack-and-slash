use super::icons::Icon;
use crate::{
	node_3d, node_3d_defaults,
	ui::{
		a11y::AKNode,
		widgets::{Text3d, WidgetShape, UNLIT_MATERIAL_ID},
		UiMat,
	},
};
use bevy::{ecs::system::EntityCommands, prelude::*, render::view::RenderLayers};
use bevy_rapier3d::parry::{math::Isometry, shape::SharedShape};
use bevy_svg::prelude::{Origin, Svg, SvgMesh3d, SvgMesh3dBundle};

#[derive(Component, Debug, Clone)]
pub struct InputIcon {
	pub icon: Icon,
	pub font: Handle<Font>,
	pub size: Vec3,
	pub flat: bool,
	pub text_entity: Entity,
	// TODO: This should be rotation + `bevy_svg::origin::Origin`
	pub isometry: Isometry<f32>,
	pub tolerance: f32,
}

impl Default for InputIcon {
	fn default() -> Self {
		Self {
			icon: default(),
			font: default(),
			size: Vec3::ONE,
			flat: true,
			text_entity: Entity::PLACEHOLDER,
			isometry: default(),
			tolerance: 0.001,
		}
	}
}

node_3d! { InputIconBundle<M: Material = UiMat> {
	input_icon: InputIcon,
	font: TextFont,
	material: MeshMaterial3d<M>,
}}

impl<M: Material> Default for InputIconBundle<M> {
	fn default() -> Self {
		node_3d_defaults! {
			input_icon: default(),
			font: default(),
			material: MeshMaterial3d(Handle::weak_from_u128(UNLIT_MATERIAL_ID)),
		}
	}
}

impl InputIcon {
	pub fn sync<M: Material>(
		mut cmds: Commands,
		mut q: Query<(Entity, &mut InputIcon, &mut AKNode, &MeshMaterial3d<M>), Changed<InputIcon>>,
		asset_server: Res<AssetServer>,
	) {
		for (id, mut this, mut ak_node, mat) in &mut q {
			let Self {
				icon: Icon {
					ref image,
					ref text,
				},
				ref font,
				size,
				flat,
				text_entity,
				isometry,
				tolerance,
			} = *this;
			cmds.get_entity(text_entity)
				.map(EntityCommands::despawn_recursive);
			let mut cmds = cmds.entity(id);
			let svg = asset_server.load::<Svg>(image);
			// `bevy_svg` doesn't insert a mesh if a handle isn't already present. PR?
			cmds.insert(SvgMesh3dBundle {
				mesh_settings: SvgMesh3d {
					svg,
					size: Some(size.xz()),
					depth: (!flat).then_some(size.y),
					rotation: Quat::from(isometry.rotation)
						* Quat::from_rotation_arc(Vec3::Y, Vec3::Z),
					origin: Origin::Center,
					tolerance,
					..default()
				},
				material: mat.clone(),
				..default()
			});
			let half_size = size * 0.5;
			cmds.insert(WidgetShape {
				shape: SharedShape::cuboid(half_size.x, half_size.y, half_size.z),
				isometry,
			});
			if let Some(text) = text.clone() {
				// FIXME: Add descriptions for icons without text
				ak_node.set_description(&*text);
				let font = font.clone();
				cmds.with_children(|cmds| {
					// Avoid re-triggering sync every frame.
					let this = this.bypass_change_detection();
					this.text_entity = cmds
						.spawn(crate::ui::widgets::Text3dBundle {
							text_3d: Text3d {
								text: text.to_string().into(),
								font,
								flat,
								vertex_scale: Vec3::new(half_size.x, half_size.y, size.z),
								..default()
							},
							material: mat.clone(),
							..default()
						})
						.id();
				});
			};
		}
	}
}
