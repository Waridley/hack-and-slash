use base64::{engine::general_purpose::URL_SAFE, prelude::*};
#[cfg(feature = "bevy_ecs")]
use bevy_ecs::prelude::Resource;
use rand::{distributions::Standard, prelude::Distribution, Rng};
#[cfg(feature = "serde")]
use serde::{Serialize, Deserialize};
use tracing::{trace, trace_span};
use std::{
	borrow::{Borrow, Cow},
	fmt::{Display, Formatter},
};

pub mod terrain;

#[cfg_attr(feature = "bevy_ecs", derive(Resource))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug, Clone)]
pub struct PlanetSeed {
	string: Cow<'static, str>,
	hash: [u8; 24],
}

impl PartialEq for PlanetSeed {
	fn eq(&self, other: &Self) -> bool {
		self.hash == other.hash
	}
}

impl Distribution<PlanetSeed> for Standard {
	fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> PlanetSeed {
		trace_span!("random PlanetSeed");
		let hash = rng.gen::<[u8; 24]>();
		trace!(hash = format!("{hash:?}"));
		let string = Cow::from(URL_SAFE.encode(hash));
		trace!(string = string.as_ref());
		PlanetSeed { string, hash }
	}
}

impl<S: Into<Cow<'static, str>>> From<S> for PlanetSeed {
	fn from(value: S) -> Self {
		trace_span!("PlanetSeed::from", ty = std::any::type_name::<S>());
		let string = value.into();
		trace!(string = string.as_ref());
		let bytes = string.as_bytes();

		{
			trace_span!("try parse as hash");
			if bytes.len() == 32 {
				let mut hash = [0; 24];
				if let Ok(n) = URL_SAFE.decode_slice(&*string, &mut hash) {
					trace!(hash = format!("{hash:?}"));
					debug_assert_eq!(n, 24);
					return Self { string, hash };
				}
			}
		}
		trace_span!("hash input");

		let bytes = if bytes.len() > Self::MAX_INPUT_LEN {
			bytes.split_at(Self::MAX_INPUT_LEN).0
		} else {
			bytes
		};

		trace!(bytes = format!("{bytes:?}"));

		// WARNING: Changing this will break compatibility with older seeds.
		let result = blake3::hash(bytes);
		let hash = (&result.as_bytes()[0..24])
			.try_into()
			.expect("blake3 outputs 32 bytes");

		trace!(hash = format!("{:?}", hash));
		Self { string, hash }
	}
}

impl Display for PlanetSeed {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		self.string.fmt(f)
	}
}

impl AsRef<str> for PlanetSeed {
	fn as_ref(&self) -> &str {
		self.string().as_ref()
	}
}

impl Borrow<str> for PlanetSeed {
	fn borrow(&self) -> &str {
		self.string.borrow()
	}
}

impl PlanetSeed {
	/// Maximum number of characters that will be hashed from the input string.
	// WARNING: Changing this will break compatibility with some older seeds.
	pub const MAX_INPUT_LEN: usize = 128;

	pub fn string(&self) -> &Cow<'static, str> {
		&self.string
	}

	/// Transforms self into its canonical representation, with the string being the base64
	/// encoding of its hash.
	pub fn canonical(mut self) -> Self {
		trace_span!("PlanetSeed into canonical");
		self.make_canonical();
		self
	}

	/// Replaces the string representation of self with the canonical base64 encoding of its hash.
	pub fn make_canonical(&mut self) {
		trace_span!("PlanetSeed::make_canonical");
		let Self { string, hash } = self;
		trace!(string = string.as_ref(), hash = format!("{hash:?}"));
		let mut canonical = String::with_capacity(32);
		URL_SAFE.encode_string(hash, &mut canonical);
		trace!(canonical);
		debug_assert_eq!(canonical.len(), 32);
		if *string != canonical {
			*string.to_mut() = canonical;
		};
		trace!(string = string.as_ref());
	}

	pub fn hash(&self) -> &[u8; 24] {
		&self.hash
	}
}

#[cfg(test)]
mod tests {
	use super::PlanetSeed;
	use breaking_attr::breaking;

	#[test]
	fn canonical_equivalence() {
		let seed = PlanetSeed::from("This is a test seed for canonical equivalence.");
		let (seed, canonical) = (seed.clone(), seed.canonical());
		assert_ne!(seed.string(), canonical.string());
		assert_eq!(seed, canonical);
	}

	#[test]
	fn auto_from_canonical() {
		let seed = rand::random::<PlanetSeed>();

		// Sanity checks
		let canon = seed.clone().canonical();
		assert_eq!(seed, canon);
		assert_eq!(seed.string(), canon.string());

		let from_canon = PlanetSeed::from(seed.string().clone());
		assert_eq!(canon, from_canon);
	}

	#[test]
	fn version_equivalence() {
		#[breaking("_44gu-Xn8ah8fqspt60ykOGP8xiM98NlS1PfVSayG9Y=")]
		const S: &str = "This is a test seed for seed consistency across game versions";
		#[breaking("BIjidzm949n7bZFRtORKPEEIN3rS7_vfvNfoqTc_GsE=")]
		const CANON: &str = "rpJXxdfel1AHU-BB7Zz8lZdZ1psLiBrU";
		let seed = PlanetSeed::from(S);
		let canon = PlanetSeed::from(CANON);
		assert_eq!(canon.string(), CANON);
		assert_eq!(canon.as_ref(), canon.clone().canonical().as_ref());
		assert_eq!(seed, canon);
	}
}
