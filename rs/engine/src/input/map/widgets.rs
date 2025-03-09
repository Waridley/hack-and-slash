use super::icons::Icon;
use crate::ui::{
	a11y::AKNode,
	widgets::{Node3d, Text3d, WidgetShape},
	UiMat,
};
use bevy::{ecs::system::EntityCommands, prelude::*};
use bevy::asset::UntypedAssetId;
use bevy::render::view::RenderLayers;
use bevy::utils::HashSet;
use bevy_rapier3d::parry::{math::Isometry, shape::SharedShape};
use bevy_svg::prelude::{Origin, Svg, SvgMesh3d, SvgMesh3dBundle};
use crate::ui::widgets::new_unlit_material;
use crate::util::{MeshOutline, PendingErasedAsset};

#[derive(Component, Debug, Clone)]
#[require(Node3d)]
pub struct InputIcon {
	pub icon: Icon,
	pub font: Handle<Font>,
	pub size: Vec3,
	pub flat: bool,
	pub text_entity: Entity,
	// TODO: This should be rotation + `bevy_svg::origin::Origin`
	pub isometry: Isometry<f32>,
	pub tolerance: f32,
	pub outline: Option<MeshOutline>,
	pub outline_material: UntypedHandle,
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
			outline: None,
			outline_material: Handle::<UiMat>::default().untyped(),
		}
	}
}

#[derive(Bundle, Default, Debug, Clone)]
pub struct InputIconBundle<M: Material = UiMat> {
	pub input_icon: InputIcon,
	pub material: MeshMaterial3d<M>,
}

impl InputIcon {
	pub fn sync<M: Material>(
		mut cmds: Commands,
		mut q: Query<(Entity, &mut InputIcon, &mut AKNode, &MeshMaterial3d<M>, &RenderLayers), Changed<InputIcon>>,
		asset_server: Res<AssetServer>,
		mut mats: ResMut<Assets<UiMat>>,
	) {
		for (id, mut this, mut ak_node, mat, layers) in &mut q {
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
				outline,
				ref outline_material,
			} = *this;
			cmds.get_entity(text_entity)
				.map(EntityCommands::despawn_recursive);
			let mut cmds = cmds.entity(id);
			let svg = asset_server.load::<Svg>(image);
			let outline_mat = PendingErasedAsset(outline_material.clone());
			cmds.insert(SvgMesh3dBundle {
				mesh_settings: SvgMesh3d {
					svg,
					size: Some(size.xz()),
					depth: (!flat).then_some(size.y),
					rotation: Quat::from(isometry.rotation)
						* Quat::from_rotation_arc(Vec3::NEG_Z, Vec3::Y),
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
			if let Some(outline) = outline {
				cmds.with_child((outline, outline_mat.clone(), layers.clone()));
			}
			if let Some(text) = text.clone() {
				// FIXME: Add descriptions for icons without text
				ak_node.set_description(&*text);
				let font = font.clone();
				cmds.with_children(|cmds| {
					// Avoid re-triggering sync every frame.
					let this = this.bypass_change_detection();
					const TEXT_SCALE: f32 = 0.5;
					let mut cmds = cmds
						.spawn((
							Text3d {
								text: text.to_string().into(),
								font,
								flat,
								// Basically sets `depth_bias` without changing material
								vertex_translation: Vec3::NEG_Y * 0.1,
								vertex_scale: size * TEXT_SCALE,
								vertex_rotation: Quat::IDENTITY,
								..default()
							},
							mat.clone(),
						));
					if let Some(outline) = outline {
						cmds.with_child((outline, outline_mat, layers.clone()));
					}
					this.text_entity = cmds.id();
				});
			};
		}
	}
}
