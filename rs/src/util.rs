use bevy_rapier3d::prelude::Real;

pub fn quantize<const BITS: u8>(angle: Real) -> Real {
	let d = angle * ((2u32.pow(BITS as u32) + 1) as Real);
	let t = d - angle;
	d - t
}
