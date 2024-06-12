#define_import_path sond_has::util

#import bevy_pbr::{
	pbr_fragment::pbr_input_from_standard_material,
	pbr_functions::{alpha_discard, calculate_view},
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
    pbr_types::STANDARD_MATERIAL_FLAGS_UNLIT_BIT,
}
#endif

struct DistanceDither {
	far_start: f32,
	far_end: f32,
	near_start: f32,
	near_end: f32,
};

@group(2) @binding(100)
var<uniform> material: DistanceDither;
@group(2) @binding(101)
var matrix_texture: texture_2d<f32>;
@group(2) @binding(102)
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

fn standard_fragment(
	in: VertexOutput,
	is_front: bool,
) -> FragmentOutput {
	// --- Modified from bevy extended_material example ---

	// generate a PbrInput struct from the StandardMaterial bindings
	var pbr_input = pbr_input_from_standard_material(in, is_front);

//	// we can optionally modify the input before lighting and alpha_discard is applied
//	pbr_input.material.base_color.b = pbr_input.material.base_color.r;

	// alpha discard
	pbr_input.material.base_color = alpha_discard(pbr_input.material, pbr_input.material.base_color);

#ifdef PREPASS_PIPELINE
	// in deferred mode we can't modify anything after that, as lighting is run in a separate fullscreen shader.
	let out = deferred_output(in, pbr_input);
#else
	// in forward mode, we calculate the lit color immediately, and then apply some post-lighting effects here.
	// in deferred mode the lit color and these effects will be calculated in the deferred lighting shader
	var out: FragmentOutput;
	if (pbr_input.material.flags & STANDARD_MATERIAL_FLAGS_UNLIT_BIT) == 0u {
		out.color = apply_pbr_lighting(pbr_input);
	} else {
		out.color = pbr_input.material.base_color;
	}

//	// we can optionally modify the lit color before post-processing is applied
//	out.color = vec4<f32>(vec4<u32>(out.color * f32(my_extended_material.quantize_steps))) / f32(my_extended_material.quantize_steps);

	// apply in-shader post processing (fog, alpha-premultiply, and also tonemapping, debanding if the camera is non-hdr)
	// note this does not include fullscreen postprocessing effects like bloom.
	out.color = main_pass_post_lighting_processing(pbr_input, out.color);

//	// we can optionally modify the final result here
//	out.color = out.color * 2.0;
#endif

	return out;
}