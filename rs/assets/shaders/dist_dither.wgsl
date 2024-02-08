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
}
#endif

struct DistanceDither {
	start: f32,
	end: f32,
};

@group(1) @binding(100)
var<uniform> material: DistanceDither;
@group(1) @binding(101)
var matrix_texture: texture_2d<f32>;
@group(1) @binding(102)
var matrix_sampler: sampler;

@fragment
fn fragment(
	in: VertexOutput,
	@builtin(front_facing) is_front: bool,
) -> FragmentOutput {
	let world_dist = length(view.world_position.xyz - in.world_position.xyz);
	let uv = in.position.xy / 16.0;
	let thresh = textureSample(matrix_texture, matrix_sampler, uv).x;
	let d = world_dist - material.start;
	let range = material.end - material.start;
	if d > thresh * range {
		discard;
	}

	// --- Modified from bevy extended_material example ---

	// generate a PbrInput struct from the StandardMaterial bindings
	var pbr_input = pbr_input_from_standard_material(in, is_front);

	// we can optionally modify the input before lighting and alpha_discard is applied
	pbr_input.material.base_color.b = pbr_input.material.base_color.r;

	// alpha discard
	pbr_input.material.base_color = alpha_discard(pbr_input.material, pbr_input.material.base_color);

#ifdef PREPASS_PIPELINE
	// in deferred mode we can't modify anything after that, as lighting is run in a separate fullscreen shader.
	let out = deferred_output(in, pbr_input);
#else
	var out: FragmentOutput;
	// apply lighting
	out.color = apply_pbr_lighting(pbr_input);

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
