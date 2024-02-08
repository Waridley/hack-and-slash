#define_import_path sond_has::util

#import bevy_pbr::{
	mesh_view_bindings::view,
}

#ifdef PREPASS_PIPELINE
#import bevy_pbr::{
    prepass_io::{VertexOutput, FragmentOutput},
    pbr_deferred_functions::deferred_output,
}
#else
#import bevy_pbr::{
    forward_io::{VertexOutput, FragmentOutput},
    pbr_functions::{apply_pbr_lighting, main_pass_post_lighting_processing},
}
#endif

struct DistanceDither {
	far_start: f32,
	far_end: f32,
	near_start: f32,
	near_end: f32,
};

@group(1) @binding(100)
var<uniform> material: DistanceDither;
@group(1) @binding(101)
var matrix_texture: texture_2d<f32>;
@group(1) @binding(102)
var matrix_sampler: sampler;

fn distance_dither(
	in: VertexOutput,
) {
	let world_dist = length(view.world_position.xyz - in.world_position.xyz);
	let uv = in.position.xy / 16.0;
	let thresh = textureSample(matrix_texture, matrix_sampler, uv).x;

	let d_far = world_dist - material.far_start;
	let far_range = material.far_end - material.far_start;
	if d_far > thresh * far_range {
		discard;
	}

	let d_near = world_dist - material.near_end;
	let near_range = material.near_start - material.near_end;
	if d_near < thresh * near_range {
		discard;
	}
}