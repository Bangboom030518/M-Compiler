use super::Type;
use proc_macro2::{Ident, TokenStream as TokenStream2};
use quote::quote;
use syn::{Field, Fields};

pub struct StructData<'a> {
    ident: &'a Ident,
    fields: syn::punctuated::IntoIter<Field>,
    include_base: bool,
    named: bool,
}

impl<'a> StructData<'a> {
    pub fn new(data: syn::DataStruct, ident: &'a Ident) -> Self {
        let mut named = false;
        let fields = match data.fields {
            Fields::Named(fields) => {
                named = true;
                fields.named
            }
            Fields::Unnamed(fields) => fields.unnamed,
            Fields::Unit => Default::default(),
        };

        Self {
            fields: fields.into_iter(),
            include_base: false,
            ident,
            named,
        }
    }

    fn get_base_declaration(&self) -> TokenStream2 {
        if self.include_base {
            quote! {
                let base = rng.gen::<Base>()
            }
        } else {
            TokenStream2::new()
        }
    }

    fn get_base_field(&mut self, field: &NamedField) -> Option<TokenStream2> {
        if !self.include_base && &field.ident_string == "base" {
            self.include_base = true;
            let ident = &field.ident;
            Some(quote! {
                #ident
            })
        } else {
            None
        }
    }

    fn get_field(&mut self, field: Field) -> TokenStream2 {
        if field.ident.is_some() {
            let field = NamedField::from(field);
            self.get_base_field(&field).unwrap_or_else(|| field.into())
        } else {
            UnnamedField::from(field).into()
        }
    }

    fn get_fields(&mut self) -> TokenStream2 {
        if *self.ident == "Identifier" {
            return quote! {
                (gen_rand_identifier(rng))
            }
        }

        let named = self.named;
        let fields = self.fields.clone().map(|field| self.get_field(field));
        if named {
            quote! {
                {
                    #(#fields),*
                }
            }
        } else {
            quote! {
                (
                    #(#fields),*
                )
            }
        }
    }
}

impl From<StructData<'_>> for TokenStream2 {
    fn from(mut data: StructData) -> Self {
        let fields = data.get_fields();
        let ident = data.ident;
        let base_declaration = data.get_base_declaration();
        quote! {
            #base_declaration;
            #ident #fields
        }
    }
}

struct NamedField {
    ident: Ident,
    ident_string: String,
    ty: Type,
}

impl NamedField {
    fn get_expr(&self) -> TokenStream2 {
        if self.ident_string.ends_with("digits") {
            quote! {
                base.rand_digits(rng)
            }
        } else {
            self.ty.clone().into()
        }
    }
}

impl From<Field> for NamedField {
    fn from(value: Field) -> Self {
        let ident = value.ident.unwrap();
        let ident_string = ident.to_string();
        Self {
            ident,
            ident_string,
            ty: Type::from(&value.ty),
        }
    }
}

impl From<NamedField> for TokenStream2 {
    fn from(value: NamedField) -> Self {
        let expr = value.get_expr();
        let ident = value.ident;
        quote! {
            #ident: #expr
        }
    }
}

struct UnnamedField {
    ty: Type,
}

impl From<Field> for UnnamedField {
    fn from(value: Field) -> Self {
        Self {
            ty: Type::from(&value.ty),
        }
    }
}

impl From<UnnamedField> for TokenStream2 {
    fn from(value: UnnamedField) -> Self {
        value.ty.into()
    }
}
