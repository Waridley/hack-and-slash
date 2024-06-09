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

#import sond_has::util::{distance_dither, matrix_texture, matrix_sampler};

struct DitherFade {
	fade: f32,
}

@group(2) @binding(200)
var<uniform> material: DitherFade;

@fragment
fn fragment(
	in: VertexOutput,
	@builtin(front_facing) is_front: bool,
) -> FragmentOutput {
	let uv = in.position.xy / 16.0;
	let thresh = textureSample(matrix_texture, matrix_sampler, uv).x;
	// We effectively need one more slot than exists in the grid in order to enable both full transparency
	// and full opacity. Otherwise one dot per tiling will be either visibile at 0.0 or invisible at 1.0
	// depending on the conditional operator used.
	let fade = material.fade * 257.0 / 256.0;
	if fade <= thresh {
		discard;
	}
	distance_dither(in);

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
