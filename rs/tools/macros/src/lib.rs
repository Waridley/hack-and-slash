use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use std::f64::consts::{PI, TAU};
use syn::{
	braced,
	parse::{Parse, ParseStream},
	parse_macro_input, parse_quote,
	spanned::Spanned,
	Error, Expr, ExprLit, FieldValue, Lit, LitInt, Member, Path, Token,
};

/// Generates a kernel for 2-dimensional convolutional blurring of type `[[f64; SIZE]; SIZE]`.
/// This allows generating the coefficients at compile-time for any size of kernel. The sum of
/// the kernel is guaranteed to be as close to 1.0 as possible, so that blurring does not alter
/// unblurred values.
///
/// ## Usage:
/// `convolution_kernel!(<KIND>, <SIZE>)`
///
/// where `<KIND>` is of the following type:
/// ```
/// enum KernelKind {
///     /// Mean blur
///     /// All cells have the same value:
///     /// `1.0 / (SIZE * SIZE)`
///     Mean,
///     /// Gaussian blur
///     /// Cells are generated by the circular Gaussian
///     /// function with the given `sigma` value.
///     Gaussian {
///         sigma: f64,
///     },
///     /// Cosine blur
///     /// Cells are generated as a function of the
///     /// cosine of their distance from the center.
///     Cosine,
/// }
/// ```
/// and `<SIZE>` is a `usize`
///
/// # Example:
/// ```
/// # fn main () {
/// # use sond_has_macros::convolution_kernel;
/// const KERNEL: [[f64; 7]; 7] = convolution_kernel!(Gaussian { sigma: 3.0 }, 7);
///
/// let sum: f64 = KERNEL.iter().flatten().sum();
/// let diff = (sum - 1.0).abs();
/// assert!(diff < 1.0e-10);
/// # }
/// ```
#[proc_macro]
pub fn convolution_kernel(input: TokenStream) -> TokenStream {
	let input = parse_macro_input!(input as Kernel);

	let mut kernel = input.get_baseline_curve();
	let sum: f64 = kernel.iter().sum();

	// Correct for inaccuracies to guarantee an integral of 1.0
	let scale = 1.0 / sum;
	for cell in &mut kernel {
		*cell *= scale
	}

	let columns = kernel.chunks_exact(input.size).map(|chunk| {
		let elements = chunk.iter();
		quote! { [ #(#elements,)* ] }
	});

	quote! {
		[ #(#columns,)* ]
	}
	.into()
}

#[derive(Copy, Clone)]
struct Kernel {
	kind: KernelKind,
	size: usize,
}

#[derive(Copy, Clone)]
enum KernelKind {
	Mean,
	Gaussian { sigma: f64 },
	Cosine,
}

impl Kernel {
	fn get_baseline_curve(self) -> Vec<f64> {
		match self.kind {
			KernelKind::Mean => std::iter::repeat(1.0 / (self.size * self.size) as f64)
				.take(self.size * self.size)
				.collect(),
			KernelKind::Gaussian { sigma } => {
				let mut ret = Vec::with_capacity(self.size * self.size);
				let r = (self.size - 1) as f64 * 0.5;
				for x in 0..self.size {
					for y in 0..self.size {
						let x = x as f64 - r;
						let y = y as f64 - r;
						let s2 = sigma * sigma;
						ret.push(1. / (TAU * s2) * (-(x * x + y * y) / (2. * s2)).exp())
					}
				}
				ret
			}
			KernelKind::Cosine => {
				let mut ret = Vec::with_capacity(self.size * self.size);
				let r = (self.size - 1) as f64 * 0.5;
				for x in 0..self.size {
					for y in 0..self.size {
						let x = x as f64 - r;
						let y = y as f64 - r;
						let len = ((x * x) + (y * y)).sqrt();
						let t = f64::min(len / r, 1.0);
						ret.push(((t * PI).cos() + 1.0) / (r * r * ((PI * PI) - 4.0) / PI))
					}
				}
				ret
			}
		}
	}
}

impl Parse for Kernel {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		use KernelKind::*;
		let kind = input.parse::<Path>()?;

		let mean: Ident = parse_quote!(Mean);
		let gaussian: Ident = parse_quote!(Gaussian);
		let cosine: Ident = parse_quote!(Cosine);

		let kind = match &kind.segments.last().as_ref().unwrap().ident {
			ident if ident == &mean => Mean,
			ident if ident == &gaussian => {
				let fields;
				braced!(fields in input);
				let fields_span = fields.span();
				let mut fields = fields
					.parse_terminated(FieldValue::parse, Token![,])?
					.into_iter();
				let Some(field) = fields.next() else {
					return Err(Error::new(fields_span, "Expected field `sigma: f64`"));
				};
				if let Some(field) = fields.next() {
					return Err(Error::new(
						field.span(),
						"Wasn't expecting more than one field for Gaussian definition",
					));
				}
				let sigma_ident: Ident = parse_quote!(sigma);
				match &field.member {
					Member::Named(ident) if ident == &sigma_ident => {}
					other => return Err(Error::new(other.span(), "Expected field `sigma: f64`")),
				}
				let Expr::Lit(ExprLit {
					lit: Lit::Float(val),
					..
				}) = &field.expr
				else {
					return Err(Error::new(field.expr.span(), "Expected a literal f64"));
				};
				let sigma = val.base10_parse()?;
				Gaussian { sigma }
			}
			ident if ident == &cosine => Cosine,
			other => {
				return Err(input.error(format!(
					"Expected {}, got {other}",
					std::any::type_name::<KernelKind>()
				)))
			}
		};

		input.parse::<Token![,]>()?;

		let size = input.parse::<LitInt>()?.base10_parse()?;

		Ok(Self { kind, size })
	}
}
