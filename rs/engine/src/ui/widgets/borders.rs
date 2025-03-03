use crate::{
	draw::{rect_points_offset, PlanarPolyLine},
	ui::widgets::{CuboidContainer, CuboidPanel},
	util::Flat,
};
use bevy::prelude::*;
use smallvec::SmallVec;

pub struct WidgetBordersPlugin;

impl Plugin for WidgetBordersPlugin {
	fn build(&self, app: &mut App) {
		app.add_systems(
			Last,
			(sync_cuboid_panel_borders, sync_cuboid_container_borders),
		);
	}
}

#[derive(Component, Debug, Clone)]
pub struct Border {
	pub cross_section: SmallVec<[Vec2; 4]>,
	pub colors: SmallVec<[SmallVec<[LinearRgba; 1]>; 2]>,
	pub margin: Vec2,
}

impl Default for Border {
	fn default() -> Self {
		Self {
			cross_section: PlanarPolyLine::default().cross_section,
			colors: PlanarPolyLine::default().colors,
			margin: Vec2::ZERO,
		}
	}
}

pub fn sync_cuboid_panel_borders(
	mut cmds: Commands,
	panels: Query<(&Children, &CuboidPanel), Changed<CuboidPanel>>,
	borders: Query<(Entity, &Border)>,
	mut meshes: ResMut<Assets<Mesh>>,
) {
	for (children, panel) in &panels {
		for (id, border) in children.iter().filter_map(|child| borders.get(*child).ok()) {
			cmds.entity(id).insert(Mesh3d(
				meshes.add(
					PlanarPolyLine {
						points: rect_points_offset(
							panel.size.x + panel.mesh_margin.x + border.margin.x,
							panel.size.z + panel.mesh_margin.z + border.margin.y,
							panel.translation.xz(),
						),
						cross_section: border.cross_section.clone(),
						colors: border.colors.clone(),
						closed: true,
					}
					.flat(),
				),
			));
		}
	}
}

pub fn sync_cuboid_container_borders(
	mut cmds: Commands,
	panels: Query<(&Children, &CuboidContainer), Changed<CuboidContainer>>,
	borders: Query<(Entity, &Border)>,
	mut meshes: ResMut<Assets<Mesh>>,
) {
	for (children, container) in &panels {
		for (id, border) in children.iter().filter_map(|child| borders.get(*child).ok()) {
			cmds.entity(id).insert(Mesh3d(
				meshes.add(
					PlanarPolyLine {
						points: rect_points_offset(
							container.size.x + border.margin.x,
							container.size.z + border.margin.y,
							container.translation.xz(),
						),
						cross_section: border.cross_section.clone(),
						colors: border.colors.clone(),
						closed: true,
					}
					.flat(),
				),
			));
		}
	}
}
