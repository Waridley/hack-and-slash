#import bevy_pbr::mesh_view_bindings
#import bevy_pbr::mesh_bindings

struct BubbleMaterial {
	@location(0) color: vec4<f32>,
	@location(1) intensity: f32,
};

@group(1) @binding(0)
var<uniform> material: BubbleMaterial;

struct FragmentInput {
	#import bevy_pbr::mesh_vertex_output
};

@fragment
fn fragment(
	in: FragmentInput
) -> @location(0) vec4<f32> {
	var N = normalize(in.world_normal);
	var V = normalize(view.world_position.xyz - in.world_position.xyz);
	var NdotV = max(dot(N, V), 0.0001);
	let glow = (1.0 + material.color.a) - pow(NdotV, material.intensity);

	return vec4(material.color.xyz * glow, glow);
}
