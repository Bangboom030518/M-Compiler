pub use enum_data::EnumData;
pub use struct_data::StructData;

mod enum_data;
mod struct_data;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use syn::{Attribute, DeriveInput, GenericArgument, Meta, PathArguments, PathSegment};

fn has_attribute(attributes: &[Attribute], name: &str) -> bool {
    for attribute in attributes {
        let attribute = match attribute.parse_meta() {
            Ok(attribute) => attribute,
            Err(_) => continue,
        };
        let Meta::Path(attribute) = attribute else {
            continue;
        };
        let Some(attribute_name) = attribute.get_ident().map(ToString::to_string) else {
            continue;
        };
        if attribute_name != name {
            continue;
        };
        return true;
    }
    false
}

#[derive(Clone)]
struct Type {
    first_segment: Option<PathSegment>,
}

impl Type {
    fn handle_vec(segment: PathSegment) -> TokenStream {
        let inner = Self::get_inner_type(&segment);
        let tokens = TokenStream::from(Self::from(inner));
        quote! {
            crate::gen_rand_vec(rng, |rng| #tokens)
        }
    }

    fn handle_box(segment: PathSegment) -> TokenStream {
        let inner = Self::get_inner_type(&segment);
        let tokens: TokenStream = Self::from(inner).into();
        quote! {
            Box::new(#tokens)
        }
    }

    fn handle_option(segment: PathSegment) -> TokenStream {
        let inner = Self::get_inner_type(&segment);
        let tokens: TokenStream = Self::from(inner).into();
        quote! {
            rng.gen::<bool>().then(|| #tokens)
        }
    }

    fn get_inner_type(segment: &PathSegment) -> &syn::Type {
        let PathArguments::AngleBracketed(arguments) = &segment.arguments else {
            unreachable!()
        };
        let GenericArgument::Type(ty) = arguments.args.first().unwrap() else {
            unreachable!()
        };
        ty
    }
}

impl From<&syn::Type> for Type {
    fn from(value: &syn::Type) -> Self {
        let mut first_segment = None;
        if let syn::Type::Path(path) = value.clone() {
            first_segment = path.path.segments.first().map(|segment| segment.clone());
        }
        Self { first_segment }
    }
}

impl From<Type> for TokenStream {
    fn from(value: Type) -> Self {
        value
            .first_segment
            .and_then(|segment| match segment.ident.to_string().as_str() {
                "Vec" => Some(Type::handle_vec(segment)),
                "String" => Some(quote! {
                    crate::gen_rand_string(rng)
                }),
                "Box" => Some(Type::handle_box(segment)),
                "Option" => Some(Type::handle_option(segment)),
                _ => None,
            })
            .unwrap_or_else(|| {
                quote! {
                    rng.gen()
                }
            })
    }
}

pub struct Data {
    ident: Ident,
    data: syn::Data,
    exclude_test: bool,
}

impl Data {
    fn get_method_tokens(&self) -> TokenStream {
        match &self.data {
            syn::Data::Enum(data) => EnumData::new(data.clone(), &self.ident).into(),
            syn::Data::Struct(data) => StructData::new(data.clone(), &self.ident).into(),
            syn::Data::Union(_) => unimplemented!("Macro doesn't works on unions"),
        }
    }

    fn get_test_tokens(&self) -> TokenStream {
        if self.exclude_test {
            return TokenStream::new();
        }

        let type_ident = &self.ident;
        let test_name = format_ident!(
            "test_{}",
            caser::Case::SnakeCase.transform(&type_ident.to_string())
        );
        quote! {
            #[test]
            fn #test_name () {
                <#type_ident as crate::prelude::Parse>::test();
            }
        }
    }
}

impl From<DeriveInput> for Data {
    fn from(value: DeriveInput) -> Self {
        Self {
            ident: value.ident,
            data: value.data,
            exclude_test: has_attribute(&value.attrs, "exclude_test"),
        }
    }
}

impl From<Data> for proc_macro::TokenStream {
    fn from(value: Data) -> Self {
        let type_ident = &value.ident;
        let method_tokens: TokenStream = value.get_method_tokens();
        let test_tokens = value.get_test_tokens();
        let tokens = quote! {
            impl rand::prelude::Distribution<#type_ident> for rand::distributions::Standard {
                fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> #type_ident {
                    #method_tokens
                }
            }

            #test_tokens
        };
        if &type_ident.to_string() == "StringLiteral" {
            std::fs::write("output.rs", tokens.to_string()).unwrap();
        }
        tokens.into()
    }
}
