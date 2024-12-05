use std::f32::consts::FRAC_PI_2;

use bevy::{
	core_pipeline::{bloom::BloomSettings, fxaa::Fxaa, tonemapping::Tonemapping, Skybox},
	ecs::system::EntityCommands,
	prelude::*,
	render::{
		camera::Viewport,
		render_asset::RenderAssetUsages,
		render_resource::{
			Extent3d, TextureDescriptor, TextureDimension, TextureFormat, TextureUsages,
			TextureViewDescriptor, TextureViewDimension,
		},
	},
};
use bevy::color::palettes::basic::FUCHSIA;
use bevy_rapier3d::{
	geometry::{Collider, CollisionGroups, Group},
	math::Vect,
	pipeline::QueryFilter,
	plugin::RapierContext,
};
use bevy_rapier3d::prelude::ShapeCastOptions;
use engine::ui::spawn_ui_camera;
use enum_components::{EntityEnumCommands, WithVariant};

use crate::{
	anim::ComponentDelta,
	planet::sky::SkyShader,
	player::{prefs::CamSmoothing, PlayerId},
	settings::Settings,
	util::LerpSlerp,
	NeverDespawn,
};

use super::{
	player_entity::{Cam, CamPivot},
	player_ui_layer, BelongsToPlayer, G1,
};

const MAX_CAM_DIST: f32 = 32.0;
const MIN_CAM_DIST: f32 = 9.6;

pub fn spawn_cameras(
	cmds: &mut Commands,
	player_id: PlayerId,
	settings: &Settings,
	images: &mut Assets<Image>,
	asset_server: &AssetServer,
	viewport: Option<Viewport>,
) {
	let (sky_texture, sky_diffuse) = {
		let size = Extent3d {
			width: 16,
			height: 96,
			depth_or_array_layers: 1,
		};
		let mut tex = Image {
			texture_descriptor: TextureDescriptor {
				label: Some("skybox_texture"),
				size,
				dimension: TextureDimension::D2,
				format: TextureFormat::Rgba32Float,
				mip_level_count: 1,
				sample_count: 1,
				usage: TextureUsages::TEXTURE_BINDING
					| TextureUsages::COPY_DST
					| TextureUsages::RENDER_ATTACHMENT,
				view_formats: &[],
			},
			texture_view_descriptor: Some(TextureViewDescriptor {
				label: Some("skybox_view"),
				dimension: Some(TextureViewDimension::Cube),
				..default()
			}),
			asset_usage: RenderAssetUsages::RENDER_WORLD,
			..default()
		};
		tex.resize(size);

		let size = Extent3d {
			width: 1,
			height: 6,
			depth_or_array_layers: 1,
		};
		let mut diffuse = Image {
			texture_descriptor: TextureDescriptor {
				label: Some("skybox_specular"),
				size,
				format: TextureFormat::Rgba8Unorm,
				..tex.texture_descriptor.clone()
			},
			texture_view_descriptor: Some(TextureViewDescriptor {
				label: Some("skybox_specular_view"),
				..tex.texture_view_descriptor.clone().unwrap()
			}),
			..tex.clone()
		};
		diffuse.resize(size);

		#[cfg(feature = "debugging")]
		for chunk in tex.data.chunks_mut(16) {
			// Displays fuchsia if shader/custom pipeline aren't working
			chunk.copy_from_slice(unsafe {
				&*(&FUCHSIA.to_f32_array() as *const [f32; 4] as *const [u8; 16])
			});
		}

		tex.reinterpret_stacked_2d_as_array(6);
		diffuse.reinterpret_stacked_2d_as_array(6);

		(images.add(tex), images.add(diffuse))
	};

	let skybox_shader = asset_server.load::<Shader>("shaders/skybox.wgsl");

	let owner = BelongsToPlayer::with_id(player_id);
	let mut cmds = cmds.spawn((
		owner,
		// Start the camera above the player in the fog, then zoom down
		TransformBundle::from_transform(Transform::from_translation(Vec3::Z * 4096.0)),
		VisibilityBundle::default(),
		Collider::ball(2.0),
		CollisionGroups::new(Group::empty(), Group::empty()),
		CamTarget::default(),
		NeverDespawn,
	));
	cmds.set_enum(Cam).with_children(|cmds| {
		// Adjusting transform of Camera entity causes weird visual glitches,
		// but parenting handles it properly
		cmds.spawn((
			Camera3dBundle {
				camera: Camera {
					hdr: true,
					..default()
				},
				transform: Transform {
					rotation: Quat::from_rotation_x(FRAC_PI_2),
					..default()
				},
				tonemapping: Tonemapping::BlenderFilmic,
				..default()
			},
			VisibilityBundle::default(),
			Skybox {
				image: sky_texture.clone(),
				brightness: 1_000.0,
			},
			SkyShader(skybox_shader),
			EnvironmentMapLight {
				diffuse_map: sky_diffuse.clone(),
				specular_map: sky_texture.clone(),
				intensity: 1_000.0,
			},
			BloomSettings {
				intensity: 0.2,
				..default()
			},
			Fxaa {
				enabled: settings.fxaa,
				..default()
			},
			NeverDespawn,
		));
	});

	spawn_ui_camera(
		cmds.commands(),
		(owner, NeverDespawn),
		player_ui_layer(player_id),
		viewport,
		player_id.get() as f64,
	);
}

pub fn spawn_pivot<'a>(
	cmds: &'a mut Commands,
	owner: BelongsToPlayer,
	crosshair: PbrBundle,
) -> EntityCommands<'a> {
	let mut cmds = cmds
		.spawn((
			owner,
			CameraVertSlider(0.125),
			TransformBundle::from_transform(Transform {
				translation: Vect::new(0.0, 0.0, MIN_CAM_DIST),
				..default()
			}),
			VisibilityBundle::default(),
		))
		.with_enum(CamPivot);
	cmds.with_children(|builder| {
		builder.spawn((owner, crosshair));
	});
	cmds
}

#[derive(Component, Debug, Default, Copy, Clone, Reflect)]
pub struct CameraVertSlider(pub f32);

#[derive(Debug, Default, Component, Deref, DerefMut)]
pub struct CamTarget(pub Transform);

pub fn position_target(
	ctx: Res<RapierContext>,
	cam_pivot_q: Query<(&GlobalTransform, &BelongsToPlayer), WithVariant<CamPivot>>,
	mut cam_q: Query<(&mut CamTarget, &Collider, &BelongsToPlayer), WithVariant<Cam>>,
) {
	for (mut target, col, cam_owner) in &mut cam_q {
		let Some(pivot_xform) = cam_pivot_q
			.iter()
			.find_map(|(xform, owner)| (owner == cam_owner).then_some(xform))
		else {
			continue;
		};

		let filter = QueryFilter::from(CollisionGroups::new(G1, !G1)).exclude_sensors();
		let (_, rot, tr) = pivot_xform.to_scale_rotation_translation();
		let dir = rot * Vect::NEG_Y;
		let pos = tr + (dir * MIN_CAM_DIST); // start at minimum distance, not player origin
		let result = ctx.cast_shape(
			pos,
			-rot,
			dir,
			col,
			ShapeCastOptions {
				max_time_of_impact: MAX_CAM_DIST - MIN_CAM_DIST,
				..default()
			},
			filter,
		);
		let toi = if let Some((_, hit)) = result {
			let toi = hit.time_of_impact;
			if toi == 0.0 {
				if ctx
					.cast_shape(
						tr + dir * MAX_CAM_DIST,
						-rot,
						-dir,
						col,
						ShapeCastOptions {
							// Don't want enormous object right in front of camera if possible
							max_time_of_impact: (MAX_CAM_DIST - MIN_CAM_DIST) * 0.3,
							..default()
						},
						filter,
					)
					.is_some()
				{
					// Max distance is still inside something, just let it be close
					toi
				} else {
					// TODO: Add hysteresis when this happens.
					// Something is in-between character and camera, but better than camera being inside it
					MAX_CAM_DIST
				}
			} else {
				toi
			}
		} else {
			MAX_CAM_DIST
		};
		target.translation = tr + dir * (toi + MIN_CAM_DIST);
		target.rotation = -rot;
	}
}

pub fn follow_target(
	player_q: Query<(&CamSmoothing, &BelongsToPlayer)>,
	mut cam_q: Query<(Entity, &mut Transform, &CamTarget, &BelongsToPlayer), WithVariant<Cam>>,
	t: Res<Time>,
	mut sender: EventWriter<ComponentDelta<Transform>>,
) {
	let dt = t.delta_seconds();
	for (id, cam_xform, target_xform, owner) in &mut cam_q {
		let Some(smoothing) = player_q
			.iter()
			.find_map(|(smoothing, id)| (*id == *owner).then_some(*smoothing))
		else {
			continue;
		};
		let s = if *smoothing <= dt {
			1.0
		} else {
			(1.0 / *smoothing) * dt
		};
		let new = cam_xform.lerp_slerp(**target_xform, s);
		sender.send(ComponentDelta::<Transform>::default_diffable(id, new));
	}
}
