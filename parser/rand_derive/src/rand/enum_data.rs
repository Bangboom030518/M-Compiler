use super::Type;
use proc_macro2::{Ident, TokenStream as TokenStream2};
use quote::quote;
use syn::{Fields, Variant};

pub struct EnumData<'a> {
    ident: &'a Ident,
    variants: syn::punctuated::IntoIter<Variant>,
}

impl<'a> EnumData<'a> {
    pub fn new(value: syn::DataEnum, ident: &'a Ident) -> Self {
        Self {
            ident,
            variants: value.variants.into_iter(),
        }
    }

    fn get_variant_expr(
        &self,
        Variant {
            ident: variant_ident,
            fields,
            ..
        }: Variant,
    ) -> TokenStream2 {
        let ident = self.ident;
        match fields {
            Fields::Unnamed(fields) => {
                let mut fields = fields.unnamed.into_iter();
                let field = fields.next().expect("Macro only works with single fields");
                let field: TokenStream2 = Type::from(&field.ty).into();
                quote! {
                    #ident::#variant_ident(#field)
                }
            }
            Fields::Unit => {
                quote! {
                    #ident::#variant_ident
                }
            }
            Fields::Named(_) => unimplemented!("Macro doesn't work with named fields."),
        }
    }

    fn get_match_branch(&self, index: usize, variant: Variant) -> TokenStream2 {
        let expr = self.get_variant_expr(variant);
        quote! {
            #index => #expr
        }
    }

    fn get_match_branches(&self) -> Vec<TokenStream2> {
        self.variants
            .clone()
            .enumerate()
            .map(|(index, variant)| self.get_match_branch(index, variant))
            .collect()
    }
}

impl From<EnumData<'_>> for TokenStream2 {
    fn from(value: EnumData) -> Self {
        let match_branches = value.get_match_branches();
        let length = value.variants.len();

        quote! {
            let choice = rand::prelude::IteratorRandom::choose(0..#length, rng).unwrap();
            match choice {
                #(#match_branches),*,
                _ => unreachable!()
            }
        }
    }
}
