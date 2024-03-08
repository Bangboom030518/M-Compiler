// use heck::ToSnakeCase;
use itertools::Itertools;
use quote::quote;
use syn::DeriveInput;

#[proc_macro_derive(BitFields)]
pub fn bit_fields(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let syn::Data::Enum(data) = input.data else {
        unimplemented!("structs and unions")
    };

    let variants = data
        .variants
        .into_iter()
        .map(|variant| {
            assert!(
                matches!(variant.fields, syn::Fields::Unit),
                "non-unit variant"
            );
            variant.ident
        })
        .collect_vec();

    let struct_name = quote::format_ident!("{name}BitFields");

    // let contains_branches = std::iter::zip(&variants, &struct_fields)
    //     .map(|(variant, field)| {
    //         quote! {
    //             #name::#variant => #struct_name.#field
    //         }
    //     })
    //     .collect_vec();

    let vis = input.vis;

    quote! {
        #[derive(PartialEq, Eq, Clone, Copy, Debug, Default)]
        #[allow(non_snake_case)]
        #vis struct #struct_name {
            #(#variants: bool),*
        }

        impl #struct_name {
            pub fn contains(self, item: #name) -> bool {
                match item {
                    #(#name::#variants => self.#variants),*
                }
            }

            pub fn clear(&mut self) {
                *self = Self::default();
            }

            pub fn insert(&mut self, item: #name) {
                match item {
                    #(#name::#variants => self.#variants = true),*
                }
            }

            pub fn hash_set(self) -> std::collections::HashSet<#name> {
                let mut output = std::collections::HashSet::new();
                #(if self.#variants {
                    output.insert(#name::#variants);
                })*
                output
            }
        }

        impl From<#struct_name> for std::collections::HashSet<#name> {
            fn from(value: #struct_name) -> Self {
                value.hash_set()
            }
        }
    }
    .into()
}
