#import bevy_render::view::View
#import bevy_render::globals::Globals

struct SkyCube {
	face_index: u32,
	face_width: u32,
	face_rotation: mat3x3<f32>,
	rotation: mat3x3<f32>,
	time_of_day: f32,
	daylight: f32,
}

@group(0) @binding(0) var<uniform> cube: SkyCube;
@group(0) @binding(1) var<uniform> view: View;
@group(0) @binding(2) var<uniform> globals: Globals;

fn ray_dir(position: vec2<f32>) -> vec3<f32> {
	return cube.face_rotation * normalize(vec3((position / (f32(cube.face_width) * 0.5)) - vec2(1.0), 1.0));
}

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
};

//  3 |  2.
//  2 |  :  `.
//  1 |  x-----x.
//  0 |  |  s  |  `.
// -1 |  0-----x.....1
//    +---------------
//      -1  0  1  2  3
//
// The axes are clip-space x and y. The region marked s is the visible region.
// The digits in the corners of the right-angled triangle are the vertex
// indices.
@vertex
fn vertex(@builtin(vertex_index) vertex_index: u32) -> VertexOutput {
    // See the explanation above for how this works.
    let clip_position = vec4(
        f32(vertex_index & 1u),
        f32((vertex_index >> 1u) & 1u),
        0.25,
        0.5
    ) * 4.0 - vec4(1.0);

    return VertexOutput(clip_position);
}

@fragment
fn fragment(in: VertexOutput) -> @location(0) vec4<f32> {
	let ray_dir = ray_dir(in.position.xy);
	let sample_dir = cube.rotation * ray_dir;
	let dl = (pow(cube.daylight, 12.0) * 8.0 + 0.001); // Full black consumes bloom color
	let sample_color = vec4(normalize((sample_dir + vec3(1.0))) * dl, 1.0);

	return sample_color;
}
