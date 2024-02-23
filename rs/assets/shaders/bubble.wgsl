#import bevy_pbr::{
    pbr_fragment::pbr_input_from_standard_material,
    pbr_functions::{
    	apply_pbr_lighting,
    	main_pass_post_lighting_processing,
    },
    forward_io::VertexOutput,
    mesh_view_bindings::view,
    mesh_bindings,
    forward_io::FragmentOutput,
}

#import sond_has::util::distance_dither;

struct BubbleMaterial {
	color: vec4<f32>,
};

@group(2) @binding(200)
var<uniform> material: BubbleMaterial;

@fragment
fn fragment(
	in: VertexOutput,
	@builtin(front_facing) is_front: bool,
) -> FragmentOutput {
	distance_dither(in);

	var pbr_in = pbr_input_from_standard_material(in, is_front);

	var N = normalize(in.world_normal);
	var V = normalize(view.world_position.xyz - in.world_position.xyz);
	var NdotV = max(dot(N, V), 0.0001);
	let glow = pow(1.0 + material.color.a - pow(NdotV, 0.3), 4.0);

	var out: FragmentOutput;

	out.color = apply_pbr_lighting(pbr_in);
	out.color += vec4(material.color.xyz * glow, 0.0);

	out.color = main_pass_post_lighting_processing(pbr_in, out.color);

	return out;
}
