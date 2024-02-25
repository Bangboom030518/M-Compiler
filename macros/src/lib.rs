use proc_macro::TokenStream;

mod spanned;

#[proc_macro_derive(Spanned, attributes(span))]
pub fn spanned(input: TokenStream) -> TokenStream {
    spanned::spanned(input.into()).into()
}
