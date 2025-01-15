use crate::declarations::{self, Declarations};
use crate::{errors, Error};
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::codegen::isa::TargetIsa;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Field {
    pub type_ref: declarations::Reference,
    pub offset: Offset32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Struct {
    pub fields: HashMap<String, Field>,
    pub size: u32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Array {
    pub length: declarations::Id,
    pub element_type: declarations::Reference,
}

impl Array {
    pub fn size(&self, declarations: &mut declarations::Declarations) -> Result<u32, Error> {
        let element_type = declarations.insert_layout_initialised(&self.element_type)?;
        let length = declarations
            .unresolved
            .get_initialised_length(self.length)?;
        let length = u32::try_from(length).map_err(|_| Error {
            span: todo!(),
            kind: errors::Kind::LengthTooBig,
        })?;
        let size = element_type.size(declarations)? * length;
        Ok(size * length)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Layout {
    Struct(Struct),
    Primitive(parser::PrimitiveKind),
    Array(Array),
}

impl Layout {
    pub fn size(&self, declarations: &mut Declarations) -> Result<u32, Error> {
        let size = match self {
            Self::Primitive(primitive) => {
                primitive.size(u32::from(declarations.isa.pointer_bytes()))
            }

            Self::Struct(Struct { size, .. }) => *size,
            Self::Array(array) => array.size(declarations)?,
        };
        Ok(size)
    }

    pub const fn expect_struct(&self) -> Result<&Struct, Error> {
        match self {
            Self::Struct(struct_layout) => Ok(struct_layout),
            _ => Err(Error {
                span: todo!(),
                kind: errors::Kind::TypeConstraintViolation {
                    constraint: errors::TypeConstraint::Struct,
                    found: todo!(),
                },
            }),
        }
    }

    pub const fn expect_array(&self) -> Result<&Array, Error> {
        match self {
            Self::Array(array) => Ok(array),
            _ => Err(Error {
                span: todo!(),
                kind: errors::Kind::TypeConstraintViolation {
                    constraint: errors::TypeConstraint::Array,
                    found: todo!(),
                },
            }),
        }
    }

    pub const fn is_aggregate(&self) -> bool {
        matches!(self, Self::Struct(_) | Self::Array(_))
    }

    pub fn cranelift_type(&self, isa: &Arc<dyn TargetIsa>) -> cranelift::prelude::Type {
        match self {
            Self::Primitive(primitive_kind) => {
                use cranelift::prelude::types;
                use parser::PrimitiveKind as P;
                match primitive_kind {
                    P::U8 | P::I8 | P::Bool => types::I8,
                    P::U16 | P::I16 => types::I16,
                    P::F32 => types::F32,
                    P::U32 | P::I32 => types::I32,
                    P::F64 => types::F64,
                    P::U64 | P::I64 => types::I64,
                    P::U128 | P::I128 => types::I128,
                    P::USize => isa.pointer_type(),
                    P::Void => todo!("void types"),
                }
            }
            Self::Struct(_) | Self::Array(_) => isa.pointer_type(),
        }
    }
}
