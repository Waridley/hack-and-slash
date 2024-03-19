use bevy::{
	app::{App, Plugin},
	asset::Handle,
	core_pipeline::{
		core_3d::graph::{Core3d, Node3d},
		Skybox,
	},
	ecs::{
		prelude::{Component, Entity},
		query::QueryItem,
		schedule::IntoSystemConfigs,
		system::{Commands, Query, Res, ResMut, Resource},
	},
	math::Vec3,
	prelude::*,
	render::{
		camera::ExtractedCamera,
		extract_component::{ExtractComponent, ExtractComponentPlugin},
		extract_resource::ExtractResourcePlugin,
		globals::{GlobalsBuffer, GlobalsUniform},
		render_asset::RenderAssets,
		render_graph::{
			NodeRunError, OutputSlotError, RenderGraphApp, RenderGraphContext, RenderLabel,
			SlotLabel, ViewNode, ViewNodeRunner,
		},
		render_resource::{
			AsBindGroup, BindGroup, BindGroupEntries, BindGroupLayout, BindGroupLayoutEntry,
			BindingType, BufferBindingType, CachedPipelineState, CachedRenderPipelineId,
			ColorTargetState, FragmentState, MultisampleState, Pipeline, PipelineCache,
			PrimitiveState, RenderPassColorAttachment, RenderPassDescriptor,
			RenderPipelineDescriptor, Shader, ShaderStages, ShaderType, SpecializedRenderPipeline,
			SpecializedRenderPipelines, TextureFormat, TextureView, TextureViewDescriptor,
			TextureViewDimension, VertexState,
		},
		renderer::{RenderContext, RenderDevice},
		texture::{FallbackImage, Image},
		view::{Msaa, ViewUniform, ViewUniformOffset, ViewUniforms},
		Extract, Render, RenderApp, RenderSet,
	},
};

use crate::planet::day_night::DayNightCycle;

pub struct SkyPlugin;

#[derive(Debug, Hash, PartialEq, Eq, Clone, RenderLabel)]
pub struct SkyGeneratorNode;

impl Plugin for SkyPlugin {
	fn build(&self, app: &mut App) {
		app.add_plugins((
			ExtractComponentPlugin::<SkyShader>::default(),
			ExtractResourcePlugin::<DayNightCycle>::default(),
		));

		let render_app = app.get_sub_app_mut(RenderApp).unwrap();
		render_app
			.add_render_graph_node::<ViewNodeRunner<SkyNode>>(Core3d, SkyGeneratorNode)
			.add_render_graph_edge(Core3d, SkyGeneratorNode, Node3d::MainOpaquePass)
			.init_resource::<SpecializedRenderPipelines<SkyPipeline>>()
			.add_systems(
				Render,
				(
					prepare_sky_pipelines.in_set(RenderSet::Prepare),
					prepare_sky_bind_groups.in_set(RenderSet::PrepareBindGroups),
				),
			);
	}

	fn finish(&self, app: &mut App) {
		let render_app = app.get_sub_app_mut(RenderApp).unwrap();
		render_app.init_resource::<SkyPipeline>();
	}
}

#[derive(Component, ExtractComponent, Clone, Debug)]
pub struct SkyShader(pub Handle<Shader>);

pub fn prepare_sky_pipelines(
	mut cmds: Commands,
	pipeline_cache: Res<PipelineCache>,
	mut pipelines: ResMut<SpecializedRenderPipelines<SkyPipeline>>,
	sky_pipeline: Res<SkyPipeline>,
	msaa: Res<Msaa>,
	q: Query<(Entity, &Skybox, &SkyShader)>,
) {
	for (id, skybox, shader) in &q {
		let pipeline = pipelines.specialize(
			&pipeline_cache,
			&sky_pipeline,
			SkyPipelineKey {
				skybox: skybox.image.clone(),
				shader: shader.0.clone(),
				msaa: msaa.samples(),
			},
		);
		cmds.entity(id).insert(SkyPipelineId(pipeline));
	}
}

#[derive(Resource)]
pub struct SkyPipeline {
	bind_group_layout: BindGroupLayout,
}

impl FromWorld for SkyPipeline {
	fn from_world(world: &mut World) -> Self {
		let render_device = world.resource::<RenderDevice>();
		let mut entries = SkyCubeUniforms::bind_group_layout_entries(render_device);
		entries.push(BindGroupLayoutEntry {
			binding: 1,
			visibility: ShaderStages::VERTEX_FRAGMENT,
			ty: BindingType::Buffer {
				ty: BufferBindingType::Uniform,
				has_dynamic_offset: true,
				min_binding_size: Some(ViewUniform::min_size()),
			},
			count: None,
		});
		entries.push(BindGroupLayoutEntry {
			binding: 2,
			visibility: ShaderStages::VERTEX_FRAGMENT,
			ty: BindingType::Buffer {
				ty: BufferBindingType::Uniform,
				has_dynamic_offset: false,
				min_binding_size: Some(GlobalsUniform::min_size()),
			},
			count: None,
		});
		Self {
			bind_group_layout: render_device
				.create_bind_group_layout("sky_bind_group_layout", &entries),
		}
	}
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct SkyPipelineKey {
	skybox: Handle<Image>,
	shader: Handle<Shader>,
	msaa: u32,
}

#[derive(Component, Debug)]
pub struct SkyPipelineId(CachedRenderPipelineId);

#[derive(Component, Debug)]
pub struct SkyViews {
	views: [TextureView; 6],
	bind_groups: [BindGroup; 6],
}

impl SpecializedRenderPipeline for SkyPipeline {
	type Key = SkyPipelineKey;

	fn specialize(&self, key: Self::Key) -> RenderPipelineDescriptor {
		RenderPipelineDescriptor {
			label: Some("sky_pipeline".into()),
			layout: vec![self.bind_group_layout.clone()],
			push_constant_ranges: vec![],
			vertex: VertexState {
				shader: key.shader.clone(),
				shader_defs: vec![],
				entry_point: "vertex".into(),
				buffers: vec![],
			},
			primitive: PrimitiveState::default(),
			depth_stencil: None,
			multisample: MultisampleState {
				count: key.msaa,
				..default()
			},
			fragment: Some(FragmentState {
				shader: key.shader,
				shader_defs: vec![],
				entry_point: "fragment".into(),
				targets: vec![Some(ColorTargetState::from(TextureFormat::Rgba32Float))],
			}),
		}
	}
}

#[derive(Default)]
pub struct SkyNode;

impl ViewNode for SkyNode {
	type ViewQuery = (
		&'static SkyPipelineId,
		&'static SkyViews,
		&'static ViewUniformOffset,
	);

	fn run(
		&self,
		_graph: &mut RenderGraphContext,
		render_context: &mut RenderContext,
		(pipeline_id, sky_views, view_uniform_offset): QueryItem<Self::ViewQuery>,
		world: &World,
	) -> Result<(), NodeRunError> {
		let pipeline_cache = world.resource::<PipelineCache>();

		let pipeline = match pipeline_cache.get_render_pipeline_state(pipeline_id.0) {
			CachedPipelineState::Queued | CachedPipelineState::Creating(_) => return Ok(()),
			CachedPipelineState::Ok(Pipeline::RenderPipeline(pipeline)) => pipeline,
			CachedPipelineState::Ok(_) => unreachable!("SkyPipeline is a RenderPipeline"),
			CachedPipelineState::Err(e) => {
				error!("{e}");
				return Ok(());
			}
		};

		{
			for i in 0..6 {
				let view = &sky_views.views[i];

				let mut render_pass =
					render_context
						.command_encoder()
						.begin_render_pass(&RenderPassDescriptor {
							label: Some("generate_sky_pass"),
							color_attachments: &[Some(RenderPassColorAttachment {
								view,
								resolve_target: None,
								ops: default(),
							})],
							..default()
						});

				render_pass.set_pipeline(pipeline);
				render_pass.set_bind_group(
					0,
					&sky_views.bind_groups[i],
					&[view_uniform_offset.offset],
				);
				render_pass.draw(0..3, 0..1);
			}
		}

		Ok(())
	}
}

pub static BIND_GROUP_LABELS: [&str; 6] = [
	"sky_bind_group_1",
	"sky_bind_group_2",
	"sky_bind_group_3",
	"sky_bind_group_4",
	"sky_bind_group_5",
	"sky_bind_group_6",
];
pub static VIEW_LABELS: [&str; 6] = [
	"sky_view_1",
	"sky_view_2",
	"sky_view_3",
	"sky_view_4",
	"sky_view_5",
	"sky_view_6",
];
pub static FACE_ROTATIONS: [Mat3; 6] = [
	Mat3 {
		x_axis: Vec3::new(0.0, 0.0, -1.0),
		y_axis: Vec3::new(0.0, 1.0, 0.0),
		z_axis: Vec3::new(1.0, 0.0, 0.0),
	},
	Mat3 {
		x_axis: Vec3::new(0.0, 0.0, 1.0),
		y_axis: Vec3::new(0.0, 1.0, 0.0),
		z_axis: Vec3::new(-1.0, 0.0, 0.0),
	},
	Mat3 {
		x_axis: Vec3::new(1.0, 0.0, 0.0),
		y_axis: Vec3::new(0.0, 0.0, 1.0),
		z_axis: Vec3::new(0.0, -1.0, 0.0),
	},
	Mat3 {
		x_axis: Vec3::new(1.0, 0.0, 0.0),
		y_axis: Vec3::new(0.0, 0.0, -1.0),
		z_axis: Vec3::new(0.0, 1.0, 0.0),
	},
	Mat3 {
		x_axis: Vec3::new(1.0, 0.0, 0.0),
		y_axis: Vec3::new(0.0, 1.0, 0.0),
		z_axis: Vec3::new(0.0, 0.0, 1.0),
	},
	Mat3 {
		x_axis: Vec3::new(-1.0, 0.0, 0.0),
		y_axis: Vec3::new(0.0, 1.0, 0.0),
		z_axis: Vec3::new(0.0, 0.0, -1.0),
	},
];

fn prepare_sky_bind_groups(
	mut commands: Commands,
	pipeline: Res<SkyPipeline>,
	view_uniforms: Res<ViewUniforms>,
	globals: Res<GlobalsBuffer>,
	images: Res<RenderAssets<Image>>,
	fallback_image: Res<FallbackImage>,
	render_device: Res<RenderDevice>,
	views: Query<(Entity, &Skybox)>,
	day_night: Res<DayNightCycle>,
	t: Res<Time>,
) {
	let s = t.elapsed_seconds_wrapped();
	let sin_t_z = (s * 0.1).sin();
	let cos_t_z = (s * 0.1).cos();
	let sin_t_x = (s * 0.25).sin();
	let cos_t_x = (s * 0.25).cos();
	let cube_rotation = Mat3 {
		x_axis: Vec3::new(sin_t_z, cos_t_z, 0.0),
		y_axis: Vec3::new(cos_t_z, -sin_t_z, 0.0),
		z_axis: Vec3::new(0.0, 0.0, 1.0),
	} * Mat3 {
		x_axis: Vec3::new(1.0, 0.0, 0.0),
		y_axis: Vec3::new(0.0, sin_t_x, cos_t_x),
		z_axis: Vec3::new(0.0, cos_t_x, -sin_t_x),
	};
	for (entity, skybox) in &views {
		if let (Some(skybox), Some(view_uniforms)) =
			(images.get(&skybox.image), view_uniforms.uniforms.binding())
		{
			let bind_groups = array_init::array_init(|i| {
				let uniforms = SkyCubeUniforms {
					face_index: i as _,
					face_width: skybox.size.x,
					face_rotation: FACE_ROTATIONS[i],
					rotation: cube_rotation,
					time_of_day: day_night.time_of_day as f32,
					daylight: day_night.daylight as f32,
					sun_position: day_night.sun_direction.normalize(),
					moon_position: day_night.moon_direction.normalize(),
				}
				.unprepared_bind_group(
					&SkyCubeUniforms::bind_group_layout(&render_device),
					&render_device,
					&images,
					&fallback_image,
				)
				.unwrap();
				render_device.create_bind_group(
					BIND_GROUP_LABELS[i],
					&pipeline.bind_group_layout,
					&BindGroupEntries::sequential((
						uniforms.bindings[0].1.get_binding(),
						view_uniforms.clone(),
						globals.buffer.binding().unwrap(),
					)),
				)
			});

			let views = array_init::array_init(|i| {
				skybox.texture.create_view(&TextureViewDescriptor {
					label: Some(VIEW_LABELS[i]),
					dimension: Some(TextureViewDimension::D2),
					base_array_layer: i as _,
					array_layer_count: Some(1),
					..default()
				})
			});

			commands
				.entity(entity)
				.insert(SkyViews { views, bind_groups });
		}
	}
}

#[derive(AsBindGroup, Default)]
pub struct SkyCubeUniforms {
	#[uniform(0)]
	face_index: u32,
	#[uniform(0)]
	face_width: f32,
	#[uniform(0)]
	face_rotation: Mat3,
	#[uniform(0)]
	rotation: Mat3,
	#[uniform(0)]
	time_of_day: f32,
	#[uniform(0)]
	daylight: f32,
	#[uniform(0)]
	sun_position: Vec3,
	#[uniform(0)]
	moon_position: Vec3,
}
