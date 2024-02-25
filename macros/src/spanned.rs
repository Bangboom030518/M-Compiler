use proc_macro2::TokenStream;
use quote::quote;
use syn::{Data, DeriveInput, Fields, Variant};

pub fn spanned(input: TokenStream) -> TokenStream {
    let DeriveInput {
        ident: name,
        generics,
        data,
        ..
    } = syn::parse2::<DeriveInput>(input).unwrap();
    let body = match data {
        Data::Union(_) => unimplemented!("unions"),
        Data::Struct(data) => {
            let field = match data.fields {
                Fields::Unnamed(fields) => {
                    let index = if fields.unnamed.len() == 1 {
                        0
                    } else {
                        fields
                            .unnamed
                            .into_iter()
                            .position(|field| {
                                field.attrs.iter().any(|attr| attr.path().is_ident("span"))
                            })
                            .expect("`span` attribute must appear on 1 field")
                    };
                    quote! {
                        self.#index
                    }
                }
                Fields::Named(fields) => {
                    let ident = if fields.named.len() == 1 {
                        fields
                            .named
                            .first()
                            .as_ref()
                            .unwrap()
                            .ident
                            .as_ref()
                            .unwrap()
                            .clone()
                    } else {
                        let field = fields
                            .named
                            .into_iter()
                            .find(|field| {
                                field.attrs.iter().any(|attr| attr.path().is_ident("span"))
                            })
                            .expect("`span` attribute must appear on 1 field");
                        field.ident.unwrap()
                    };
                    quote! {
                        self.#ident
                    }
                }
                Fields::Unit => unimplemented!("unit structs"),
            };
            quote! {
                crate::Spanned::span(#field)
            }
        }
        Data::Enum(data) => {
            let variants = data
                .variants
                .into_iter()
                .map(|Variant { ident, fields, .. }| {
                    let Fields::Unnamed(fields) = fields else {
                        unimplemented!("named and unit fields on variants")
                    };
                    assert_eq!(
                        fields.unnamed.len(),
                        1,
                        "variants should have exactly 1 field"
                    );
                    quote! {
                        #name::#ident(value) => crate::Spanned::span(value)
                    }
                });
            quote! {
                match &self {
                    #(#variants),*
                }
            }
        }
    };
    quote! {
        impl<#generics> crate::Spanned for #name<#generics> {
            fn span(&self) -> tokenizer::Span {
                #body
            }
        }
    }
}
