use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
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
            let fields = match data.fields {
                Fields::Unnamed(fields) => fields.unnamed,
                Fields::Named(fields) => fields.named,
                Fields::Unit => unimplemented!("unit structs"),
            };

            fields
                .iter()
                .enumerate()
                .find(|(_, field)| {
                    field.attrs.iter().any(|attr| {
                        attr.path().is_ident("span") && attr.meta.require_path_only().is_ok()
                    })
                })
                .map(|(index, field)| {
                    let field = match &field.ident {
                        Some(ident) => ident.into_token_stream(),
                        None => syn::Index::from(index).into_token_stream(),
                    };
                    quote! {
                        crate::Spanned::span(&self.#field)
                    }
                })
                .unwrap_or_else(|| {
                    let start = fields
                        .iter()
                        .enumerate()
                        .find(|(_, field)| {
                            field.attrs.iter().any(|attr| {
                                attr.path().is_ident("span")
                                    && syn::parse2::<syn::Ident>(
                                        attr.meta.require_list().unwrap().tokens.clone(),
                                    )
                                    .unwrap()
                                    .to_string()
                                        == "start"
                            })
                        })
                        .map(|(index, field)| {
                            let field = match &field.ident {
                                Some(ident) => ident.into_token_stream(),
                                None => syn::Index::from(index).into_token_stream(),
                            };

                            quote! {
                                crate::Spanned::start(&self.#field)
                            }
                        })
                        .expect("no `span` or `start` attribute");

                    let end = fields
                        .into_iter()
                        .enumerate()
                        .find(|(_, field)| {
                            field.attrs.iter().any(|attr| {
                                attr.path().is_ident("span")
                                    && syn::parse2::<syn::Ident>(
                                        attr.meta.require_list().unwrap().tokens.clone(),
                                    )
                                    .unwrap()
                                    .to_string()
                                        == "end"
                            })
                        })
                        .map(|(index, field)| {
                            let field = match &field.ident {
                                Some(ident) => ident.into_token_stream(),
                                None => syn::Index::from(index).into_token_stream(),
                            };

                            quote! {
                                crate::Spanned::end(&self.#field)
                            }
                        })
                        .expect("no `end` for `start`");

                    quote! {
                        (#start)..(#end)
                    }
                })
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
                        #name::#ident(value) => crate::Spanned::span(&value)
                    }
                });

            quote! {
                match self {
                    #(#variants),*
                }
            }
        }
    };

    // panic!("{}", &body);

    quote! {
        impl<#generics> crate::Spanned for #name<#generics> {
            fn span(&self) -> tokenizer::Span {
                #body
            }
        }
    }
}
