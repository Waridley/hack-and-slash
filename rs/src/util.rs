#[inline(always)]
pub const fn truncate_mantissa<const BITS: u32>(value: f32) -> f32 {
	debug_assert!(BITS < f32::MANTISSA_DIGITS);
	let mask = u32::MAX << (f32::MANTISSA_DIGITS - BITS);
	unsafe {
		// f32::to_bits and f32::from_bits are const unstable
		// check their source code for why this is safe on all platforms we care about
		std::mem::transmute(std::mem::transmute::<_, u32>(value) & mask)
	}
}
