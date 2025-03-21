use crate::{offloading::wasm_yield, planet::PlanetVec2};
use macros::convolution_kernel;
use noise::{
	core::worley::{distance_functions, worley_2d, ReturnType},
	permutationtable::PermutationTable,
	NoiseFn, Seedable,
};
use ordered_float::OrderedFloat;
use std::sync::Arc;
use web_time::{Duration, Instant};

type OF64 = OrderedFloat<f64>;

const KERNEL: [[f64; 25]; 25] = convolution_kernel!(Cosine, 25);

pub struct ChooseAndSmooth<const N: usize> {
	pub sources: [Source; N],
}

impl<const N: usize> ChooseAndSmooth<N> {
	pub fn new(sources: [Source; N]) -> Self {
		Self { sources }
	}

	fn strongest_at(&self, point: [f64; 2]) -> usize {
		self.sources
			.iter()
			.map(|source| OF64::from(source.strength.get(point)))
			.enumerate()
			.max_by_key(|(_, val)| *val)
			.unwrap()
			.0
	}

	// Needs to be async to allow splitting work over multiple frames on WASM
	pub async fn generate_map(&self, center: PlanetVec2, rows: usize, columns: usize) -> Vec<f32> {
		let mut last_await = Instant::now();

		// Extra space around edges for blurring
		let winner_rows = rows + KERNEL.len() - 1;
		let winner_cols = columns + KERNEL.len() - 1;

		let mut winners = Vec::with_capacity(winner_rows * winner_cols);
		let mut ret = Vec::with_capacity(rows * columns);

		// map range to eliminate seams.
		// e.g. `0..128 => -64.0..=64.0`
		let stretch = PlanetVec2::new(
			(columns as f64) / ((columns - 1) as f64),
			(rows as f64) / ((rows - 1) as f64),
		);

		for x in 0..winner_cols {
			for y in 0..winner_rows {
				winners.push(self.strongest_at([
					center.x + (x as f64 * stretch.x) - (winner_cols as f64 * 0.5),
					center.y + (-(y as f64) * stretch.y) + (winner_rows as f64 * 0.5),
				]) as u8)
			}
			if Instant::now().duration_since(last_await) > Duration::from_micros(500) {
				wasm_yield().await;
				last_await = Instant::now();
			}
		}

		for x in 0..columns {
			for y in 0..rows {
				let point = [
					center.x + (x as f64 * stretch.x) - (columns as f64 * 0.5),
					// HeightField has to be inverted for chunk indices to follow PlanetVec2
					center.y + (-(y as f64) * stretch.y) + (rows as f64 * 0.5),
				];
				let mut value_caches = [None; N];
				let mut sum = 0.0;

				// Gaussian blur kernel convolution
				for j in 0..KERNEL.len() {
					for i in 0..KERNEL.len() {
						let strongest = winners[((x + j) * winner_cols) + y + i] as usize;
						sum += *value_caches[strongest]
							.get_or_insert_with(|| self.sources[strongest].noise.get(point))
							* KERNEL[i][j]
					}
				}
				ret.push(sum as f32)
			}
			if Instant::now().duration_since(last_await) > Duration::from_micros(500) {
				wasm_yield().await;
				last_await = Instant::now();
			}
		}

		ret
	}
}

impl<const N: usize> NoiseFn<f64, 2> for ChooseAndSmooth<N> {
	fn get(&self, point: [f64; 2]) -> f64 {
		let mut value_caches = [None; N];
		let mut sum = 0.0;

		// Gaussian blur kernel convolution
		let r = (KERNEL.len() - 1) as f64;
		for (j, col) in KERNEL.iter().enumerate() {
			for (i, value) in col.iter().enumerate() {
				let cell = [point[0] + j as f64 - r, point[1] + i as f64 - r];
				let strongest = self.strongest_at(cell);
				sum += *value_caches[strongest]
					.get_or_insert_with(|| self.sources[strongest].noise.get(point))
					/ value
			}
		}
		sum
	}
}

pub struct Source {
	pub noise: Box<dyn NoiseFn<f64, 2> + Send + Sync>,
	pub strength: Box<dyn NoiseFn<f64, 2> + Send + Sync>,
}

impl Source {
	pub fn new(
		source: impl NoiseFn<f64, 2> + Send + Sync + 'static,
		strength: impl NoiseFn<f64, 2> + Send + Sync + 'static,
	) -> Self {
		Self {
			noise: Box::new(source),
			strength: Box::new(strength),
		}
	}
}

/// Workaround for the fact that `::noise::Worley` contains a non-Send/Sync distance function pointer.
/// Copied from `noise` crate, but with  `Rc<dyn Fn(&[f64], &[f64]) -> f64>` replaced by `Arc<
/// dyn Fn(&[f64], &[f64]) -> f64 + Send + Sync>`
pub struct SyncWorley {
	/// Specifies the distance function to use when calculating the boundaries of
	/// the cell.
	pub distance_function: Arc<DistanceFunction>,

	/// Signifies whether the distance from the borders of the cell should be returned, or the
	/// value for the cell.
	pub return_type: ReturnType,

	/// Frequency of the seed points.
	pub frequency: f64,

	seed: u32,
	perm_table: PermutationTable,
}

pub type DistanceFunction = dyn Fn(&[f64], &[f64]) -> f64 + Send + Sync;

impl SyncWorley {
	pub const DEFAULT_SEED: u32 = 0;
	pub const DEFAULT_FREQUENCY: f64 = 1.0;

	pub fn new(seed: u32) -> Self {
		Self {
			perm_table: PermutationTable::new(seed),
			seed,
			distance_function: Arc::new(distance_functions::euclidean),
			return_type: ReturnType::Value,
			frequency: Self::DEFAULT_FREQUENCY,
		}
	}

	/// Sets the distance function used by the Worley cells.
	pub fn set_distance_function<F>(self, function: F) -> Self
	where
		F: Fn(&[f64], &[f64]) -> f64 + Send + Sync + 'static,
	{
		Self {
			distance_function: Arc::new(function),
			..self
		}
	}

	/// Enables or disables applying the distance from the nearest seed point
	/// to the output value.
	pub fn set_return_type(self, return_type: ReturnType) -> Self {
		Self {
			return_type,
			..self
		}
	}

	/// Sets the frequency of the seed points.
	pub fn set_frequency(self, frequency: f64) -> Self {
		Self { frequency, ..self }
	}
}

impl Default for SyncWorley {
	fn default() -> Self {
		Self::new(Self::DEFAULT_SEED)
	}
}

impl Seedable for SyncWorley {
	/// Sets the seed value used by the Worley cells.
	fn set_seed(self, seed: u32) -> Self {
		// If the new seed is the same as the current seed, just return self.
		if self.seed == seed {
			return self;
		}

		// Otherwise, regenerate the permutation table based on the new seed.
		Self {
			perm_table: PermutationTable::new(seed),
			seed,
			..self
		}
	}

	fn seed(&self) -> u32 {
		self.seed
	}
}

impl NoiseFn<f64, 2> for SyncWorley {
	fn get(&self, point: [f64; 2]) -> f64 {
		worley_2d(
			&self.perm_table,
			&*self.distance_function,
			self.return_type,
			[point[0] * self.frequency, point[1] * self.frequency].into(),
		)
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn kernel_integral_1() {
		let sum: f64 = KERNEL.iter().flatten().sum();
		let diff = (sum - 1.0).abs();
		assert!(diff < 1.0e-10, "{KERNEL:?}\nΣ={sum}");
	}
}
