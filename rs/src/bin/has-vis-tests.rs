use bevy::core_pipeline::clear_color::ClearColorConfig;
use bevy::prelude::*;
use bevy::window::close_on_esc;
use bevy_rapier3d::prelude::*;
use bevy_rapier3d::render::RapierDebugRenderPlugin;

#[bevy_main]
fn main() {
	sond_has::tests::app()
		.add_plugins(DefaultPlugins)
		.add_plugin(RapierDebugRenderPlugin::default())
		.add_startup_system(setup)
		.add_system(close_on_esc)
		.run()
}

fn setup(mut cmds: Commands) {
	let cam_pos = Vec3::new(0.0, -10.0, 2.0);
	cmds.spawn(Camera3dBundle {
		transform: Transform {
			translation: cam_pos,
			rotation: Quat::from_rotation_arc(
				Vec3::NEG_Z,
				-cam_pos.normalize()
			),
			..default()
		},
		camera_3d: Camera3d {
			clear_color: ClearColorConfig::Custom(Color::BLACK),
			..default()
		},
		..default()
	});
	cmds.spawn((
		RigidBody::KinematicPositionBased,
		Collider::ball(0.5),
	));
}
