#![warn(clippy::pedantic, clippy::nursery)]

use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

mod rand;

#[proc_macro_derive(Rand, attributes(exclude_test, skip_variant, default_variant))]
pub fn derive_rand(input: TokenStream) -> TokenStream {
    let tree = parse_macro_input!(input as DeriveInput);
    rand::Data::from(tree).into()
}
