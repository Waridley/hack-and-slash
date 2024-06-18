#import bevy_pbr::{
	mesh_view_bindings::view,
}

#ifdef PREPASS_PIPELINE
#import bevy_pbr::{
    prepass_io::{VertexOutput, FragmentOutput},
}
#else
#import bevy_pbr::{
    forward_io::{VertexOutput, FragmentOutput},
    pbr_functions::{apply_pbr_lighting, main_pass_post_lighting_processing},
}
#endif

#import sond_has::util::{distance_dither, standard_fragment, matrix_texture, matrix_sampler};

struct DitherFade {
	fade: f32,
#ifdef SIXTEEN_BYTE_ALIGNMENT
	_wasm_padding_8b: u32,
	_wasm_padding_12b: u32,
	_wasm_padding_16b: u32,
#endif
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

	return standard_fragment(in, is_front);
}
