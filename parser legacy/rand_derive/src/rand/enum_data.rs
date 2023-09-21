use super::{has_attribute, Type};
use proc_macro2::{Ident, TokenStream as TokenStream2};
use quote::quote;
use syn::{Fields, Variant};

pub struct EnumData<'a> {
    ident: &'a Ident,
    variants: Vec<Variant>,
    default_variant: Variant,
}

impl<'a> EnumData<'a> {
    pub fn new(value: syn::DataEnum, ident: &'a Ident) -> Self {
        let mut variants = Vec::new();
        let mut default_variant = None;
        for variant in value.variants {
            if !has_attribute(&variant.attrs, "skip_variant") {
                variants.push(variant);
                continue;
            }
            if has_attribute(&variant.attrs, "default_variant") {
                assert!(
                    default_variant.is_none(),
                    "There should only be one `#[default_variant]` attribute used."
                );
                default_variant = Some(variant);
            }
        }
        Self {
            ident,
            default_variant: default_variant.unwrap_or_else(|| {
                variants
                    .first()
                    .expect("`#[default_variant]` attribute needed on a variant")
                    .clone()
            }),
            variants,
        }
    }

    fn get_variant_expr(
        &self,
        Variant {
            ident: variant_ident,
            fields,
            ..
        }: &Variant,
    ) -> TokenStream2 {
        let ident = self.ident;
        match fields {
            Fields::Unnamed(fields) => {
                let mut fields = fields.unnamed.iter();
                let field = fields.next().expect("Macro only works with single fields");
                let field: TokenStream2 = Type::from(&field.ty).into();
                quote! {
                    #ident::#variant_ident(#field)
                }
            }
            Fields::Unit => quote! {
                #ident::#variant_ident
            },
            Fields::Named(_) => unimplemented!("Macro doesn't work with named fields."),
        }
    }

    fn get_match_branch(&self, index: usize, variant: &Variant) -> TokenStream2 {
        let expr = self.get_variant_expr(&variant);
        quote! {
            #index => #expr,
        }
    }

    fn get_match_branches(&self) -> Vec<TokenStream2> {
        self.variants
            .clone()
            .into_iter()
            .enumerate()
            .map(|(index, variant)| self.get_match_branch(index, &variant))
            .collect()
    }
}

impl From<EnumData<'_>> for TokenStream2 {
    fn from(value: EnumData) -> Self {
        let match_branches = value.get_match_branches();
        let default_expr = value.get_variant_expr(&value.default_variant);
        let length = value.variants.len();
        quote! {
            let choice = rand::prelude::IteratorRandom::choose(0..#length, rng).unwrap();
            match choice {
                #(#match_branches)*
                _ => #default_expr,
            }
        }
    }
}
