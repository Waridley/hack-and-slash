use std::f32::consts::FRAC_PI_2;

use bevy::{
	core_pipeline::{bloom::BloomSettings, fxaa::Fxaa, tonemapping::Tonemapping, Skybox},
	ecs::system::{EntityCommands, Res},
	prelude::*,
	render::{
		camera::{ManualTextureViewHandle, ManualTextureViews, RenderTarget},
		render_resource::{
			Extent3d, TextureDescriptor, TextureDimension, TextureFormat, TextureUsages,
			TextureViewDescriptor, TextureViewDimension,
		},
	},
	transform::components::{GlobalTransform, Transform},
	window::WindowRef,
};
use bevy_rapier3d::{
	geometry::{Collider, CollisionGroups, Group},
	math::Vect,
	pipeline::QueryFilter,
	plugin::RapierContext,
};
use enum_components::{ERef, EntityEnumCommands};

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
	BelongsToPlayer, G1,
};

const MAX_CAM_DIST: f32 = 32.0;
const MIN_CAM_DIST: f32 = 9.6;

pub fn spawn_camera<'w, 's, 'a>(
	cmds: &'a mut Commands<'w, 's>,
	player_id: PlayerId,
	settings: &Settings,
	images: &mut Assets<Image>,
	asset_server: &AssetServer,
	manual_texture_views: &ManualTextureViews,
) -> EntityCommands<'w, 's, 'a> {
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

		for chunk in tex.data.chunks_mut(4) {
			// Displays fuchsia if shader/custom pipeline aren't working
			chunk.copy_from_slice(&Color::FUCHSIA.as_rgba_u8());
		}
		/* Looks washed out. Using AmbientLight instead. */
		// for chunk in diffuse.data.chunks_mut(4) {
		// 	chunk.copy_from_slice(&Color::rgb(0.24, 0.08, 0.48).as_rgba_u8());
		// }

		tex.reinterpret_stacked_2d_as_array(6);
		diffuse.reinterpret_stacked_2d_as_array(6);

		(images.add(tex), images.add(diffuse))
	};

	let skybox_shader = asset_server.load::<Shader>("shaders/skybox.wgsl");

	let mut cmds = cmds.spawn((
		BelongsToPlayer::with_id(player_id),
		// Start the camera above the player in the fog, then zoom down
		TransformBundle::from_transform(Transform::from_translation(Vec3::Z * 4096.0)),
		Collider::ball(2.0),
		CollisionGroups::new(Group::empty(), Group::empty()),
		CamTarget::default(),
		NeverDespawn,
	));
	cmds.set_enum(Cam).with_children(|builder| {
		let manual_tv_handle = ManualTextureViewHandle(player_id.get() as u32 - 1);
		// Adjusting transform of Camera entity causes weird visual glitches,
		// but parenting handles it properly
		builder.spawn((
			Camera3dBundle {
				camera: Camera {
					hdr: true,
					target: if manual_texture_views.contains_key(&manual_tv_handle) {
						RenderTarget::TextureView(manual_tv_handle)
					} else {
						RenderTarget::Window(WindowRef::Primary)
					},
					..default()
				},
				transform: Transform {
					rotation: Quat::from_rotation_x(FRAC_PI_2),
					..default()
				},
				tonemapping: Tonemapping::BlenderFilmic,
				..default()
			},
			Skybox(sky_texture.clone()),
			SkyShader(skybox_shader),
			EnvironmentMapLight {
				diffuse_map: sky_diffuse.clone(),
				specular_map: sky_texture.clone(),
			},
			BloomSettings {
				intensity: 0.1,
				..default()
			},
			Fxaa {
				enabled: settings.fxaa,
				..default()
			},
			NeverDespawn,
		));
	});
	cmds
}

pub fn spawn_pivot<'w, 's, 'a>(
	cmds: &'a mut Commands<'w, 's>,
	owner: BelongsToPlayer,
	crosshair: PbrBundle,
) -> EntityCommands<'w, 's, 'a> {
	let mut cmds = cmds
		.spawn((
			owner,
			CameraVertSlider(0.125),
			TransformBundle::from_transform(Transform {
				translation: Vect::new(0.0, 0.0, MIN_CAM_DIST),
				..default()
			}),
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
	cam_pivot_q: Query<(&GlobalTransform, &BelongsToPlayer), ERef<CamPivot>>,
	mut cam_q: Query<(&mut CamTarget, &Collider, &BelongsToPlayer), ERef<Cam>>,
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
			MAX_CAM_DIST - MIN_CAM_DIST,
			true,
			filter,
		);
		let toi = if let Some((_, toi)) = result {
			let toi = toi.toi;
			if toi == 0.0 {
				if ctx
					.cast_shape(
						tr + dir * MAX_CAM_DIST,
						-rot,
						-dir,
						col,
						(MAX_CAM_DIST - MIN_CAM_DIST) * 0.3, // Don't want enormous object right in front of camera if possible
						true,
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
	mut cam_q: Query<(Entity, &mut Transform, &CamTarget, &BelongsToPlayer), ERef<Cam>>,
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
		sender.send(ComponentDelta::<Transform>::default_diffable(id, new))
	}
}
