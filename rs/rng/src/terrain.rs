use super::PlanetSeed;
use tracing::info;
use rand::{distributions::Standard, prelude::Distribution, Rng, SeedableRng};
use rand_chacha::ChaCha8Rng;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(C)]
pub struct TerrainSeeds {
	base: [u32; 2],
	perlin: HSSeed,
	worley: HSSeed,
	billow: HSSeed,
	ridged: HSSeed,
}

impl From<&PlanetSeed> for TerrainSeeds {
	fn from(value: &PlanetSeed) -> Self {
		let mut seed = [0; 32];
		seed[0..24].copy_from_slice(value.hash());

		// WARNING: Changing these will break compatibility with older seeds.
		let mut rng = ChaCha8Rng::from_seed(seed);

		let base = rng.gen();

		let perlin = rng.gen::<HSSeed>();

		let mut worley = rng.gen::<HSSeed>();
		let mut attempt = 0;
		while attempt < 1024 {
			// Best effort at avoiding strength collisions.
			if perlin.strength == worley.strength {
				worley = rng.gen();
			} else {
				break;
			}
			attempt += 1;
		}
		if attempt > 0 {
			info!(?perlin, "Worley clashed with Perlin {attempt} time(s)");
		}

		let mut billow = rng.gen::<HSSeed>();
		let mut attempt = 0;
		while attempt < 1024 {
			// Best effort at avoiding strength collisions.
			if [perlin.strength, worley.strength].contains(&billow.strength) {
				billow = rng.gen();
			} else {
				break;
			}
			attempt += 1;
		}
		if attempt > 0 {
			info!(
				?perlin,
				?worley,
				"Billow clashed with Perlin or Worley {attempt} time(s)"
			);
		}

		let mut ridged = rng.gen::<HSSeed>();
		let mut attempt = 0;
		while attempt < 1024 {
			// Best effort at avoiding strength collisions.
			if [perlin.strength, worley.strength, billow.strength].contains(&ridged.strength) {
				ridged = rng.gen();
			} else {
				break;
			}
			attempt += 1;
		}
		if attempt > 0 {
			info!(
				?perlin,
				?worley,
				?billow,
				"Ridged clashed with Perlin, Worley, or Billow {attempt} time(s)"
			);
		}

		Self {
			base,
			perlin,
			worley,
			billow,
			ridged,
		}
	}
}

impl TerrainSeeds {
	pub fn base(&self) -> [u32; 2] {
		self.base
	}
	pub fn perlin(&self) -> HSSeed {
		self.perlin
	}
	pub fn worley(&self) -> HSSeed {
		self.worley
	}
	pub fn billow(&self) -> HSSeed {
		self.billow
	}
	pub fn ridged(&self) -> HSSeed {
		self.ridged
	}
}

/// Combined heights and strength seed for [ChooseAndSmooth](terrain::noise::ChooseAndSmooth)
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(C)]
pub struct HSSeed {
	heights: u32,
	strength: u32,
}

impl Distribution<HSSeed> for Standard {
	fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> HSSeed {
		HSSeed {
			heights: rng.next_u32(),
			strength: rng.next_u32(),
		}
	}
}

impl HSSeed {
	pub fn heights(&self) -> u32 {
		self.heights
	}
	pub fn strength(&self) -> u32 {
		self.strength
	}
}

#[cfg(test)]
mod tests {
	use crate::PlanetSeed;
	use super::{HSSeed, TerrainSeeds};
	use breaking_attr::breaking;

	#[test]
	fn seed_equivalence() {
		let seed = rand::random::<PlanetSeed>();
		let a = TerrainSeeds::from(&seed);
		let b = TerrainSeeds::from(&seed);
		assert_eq!(a, b);
	}

	#[test]
	fn version_equivalence() {
		#[breaking("G5tz99mmq0yIJhh4FrWvaL7WADXYbCyNcwvChZAwtbg=")]
		const SEED: &str = "This is a PlanetSeed for testing TerrainSeed equivalence across versions.";
		let seed = PlanetSeed::from(SEED);
		#[breaking("UexE26eb5tYkAcQI0B8qYGJwRchfbKvM2byviFz52wg=")]
		const CANON: &str = "dif3XzLPR0QkeiIeiDUZS_fAfVYwGsLv";
		assert_eq!(
			seed.clone().canonical().string(),
			CANON,
		);
		// Note: If sources are added or removed, the sources shared between versions must remain
		// equivalent, but it is fine to add the values for the new sources or remove old ones here.
		#[breaking("JJMBtPGINwVPoEi8tw-Y_j5l3iIquDu9pfOcPFZvR7s=")]
		const TERRAIN_SEEDS: TerrainSeeds = TerrainSeeds {
			base: [3921923909, 2705971270],
			perlin: HSSeed {
				heights: 2112551709,
				strength: 1762326461
			},
			worley: HSSeed {
				heights: 2032743752,
				strength: 3889465128
			},
			billow: HSSeed {
				heights: 1264812115,
				strength: 391812050
			},
			ridged: HSSeed {
				heights: 2995831856,
				strength: 2095380095
			},
		};
		assert_eq!(
			TerrainSeeds::from(&seed),
			TERRAIN_SEEDS,
		);
	}
}
