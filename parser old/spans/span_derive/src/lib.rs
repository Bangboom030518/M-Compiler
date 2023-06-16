use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Fields};

#[proc_macro_derive(Span)]
pub fn derive_span(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let generics = input.generics;
    let function_content = match input.data {
        Data::Enum(data) => {
            let variants = data.variants.into_iter().map(|variant| {
                let variant_name = variant.ident;
                let fields = match variant.fields {
                    Fields::Unnamed(fields) => fields.unnamed.into_iter(),
                    _ => panic!("Fields in enum variant should be unnamed"),
                };
                assert_eq!(
                    fields.len(),
                    1,
                    "Every variant in Spanned enum should have exactly 1 field"
                );
                quote!(Self :: #variant_name ( item ) => span::Spanned::as_span(item))
            });
            quote! {
                match self {
                    #(#variants),*
                }
            }
        }
        Data::Struct(data) => {
            match data.fields {
                Fields::Named(named) => named.named,
                _ => panic!("Fields in struct should be named"),
            };
            quote!(self.span)
        }
        Data::Union(_) => unimplemented!("Cannot derive Span onto union."),
    };
    quote! {
        impl #generics span::Spanned for #name #generics {
            fn as_span(&self) -> span::Span {
                #function_content
            }
        }
    }
    .into()
}
