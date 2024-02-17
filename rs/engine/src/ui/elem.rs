use bevy::ecs::query::QueryEntityError;
use bevy::prelude::*;

#[derive(Component, Deref)]
pub struct RealDomNode(Option<Entity>);

#[derive(Component, Deref)]
pub struct VDomNode(Entity);

#[derive(Component)]
pub struct Slider {
	pub value: f32,
	pub start: Transform,
	pub end: Transform,
}

fn new_real_slider(cmds: &mut Commands, slider: &Slider) -> Entity {
	cmds.spawn((SpriteSheetBundle { ..default() },))
		.with_children(|builder| {
			builder.spawn((SpriteSheetBundle { ..default() },));
		})
		.id()
}

pub fn update_sliders(
	mut cmds: Commands,
	mut vdom: Query<(Entity, &mut Slider, &mut RealDomNode)>,
	mut dom: Query<(Entity, &mut Transform, &VDomNode)>,
) {
	for (id, slider, real) in &mut vdom {
		if !slider.is_changed() {
			continue;
		}
		if let Some(real) = *real {
			let (_, mut xform, _) = match dom.get_mut(real) {
				Ok(real) => real,
				Err(e) => {
					error!("{e}");
					continue;
				} // TODO: May want to only error once, and re-spawn if it happens again next frame
			};

			todo!()
		} else {
			real.0 = Some(new_real_slider(&mut cmds, &*slider));
			continue;
		}
	}
	for (id, mut xform, virt) in &mut dom {
		if !xform.is_changed() {
			continue;
		}
		let (_, mut slider, _) = match vdom.get_mut(**virt) {
			Ok(virt) => virt,
			Err(QueryEntityError::NoSuchEntity(_)) => {
				warn!("Missing VDom slider. Real slider should already have been despawned. Despawning now.");
				cmds.entity(id).despawn_recursive();
				continue;
			}
			Err(e) => {
				error!("{e} -- Despawning real slider");
				cmds.entity(id).despawn_recursive();
				continue;
			}
		};

		todo!()
	}
}

#[derive(Component)]
pub struct Label {
	pub text: Text,
}
