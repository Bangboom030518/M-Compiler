use crate::declarations::{self, Declarations, ScopeId};
use crate::SemanticError;
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::codegen::isa::TargetIsa;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Field {
    pub type_ref: declarations::TypeReference,
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
    pub element_type: declarations::TypeReference,
}

impl Array {
    pub fn size(
        &self,
        declarations: &mut declarations::Declarations,
    ) -> Result<u32, SemanticError> {
        let element_type = declarations
            .insert_layout_initialised(&self.element_type, declarations::random_scope())?;
        let length = declarations.get_length(self.length)?;
        let length = u32::try_from(length).map_err(|_| SemanticError::LengthTooBig)?;
        let size = element_type.size(declarations)? * length;
        Ok(size * length)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Layout {
    Struct(Struct),
    Primitive(Primitive),
    Array(Array),
    Void,
}

impl Layout {
    pub fn size(&self, declarations: &mut Declarations) -> Result<u32, SemanticError> {
        let size = match self {
            Self::Primitive(primitive) => {
                primitive.size(u32::from(declarations.isa.pointer_bytes()))
            }

            Self::Struct(Struct { size, .. }) => *size,
            Self::Array(array) => array.size(declarations)?,
            Self::Void => 0,
        };
        Ok(size)
    }

    pub const fn expect_struct(&self) -> Result<&Struct, SemanticError> {
        match self {
            Self::Struct(struct_layout) => Ok(struct_layout),
            _ => Err(SemanticError::ExpectedStruct),
        }
    }

    pub const fn expect_array(&self) -> Result<&Array, SemanticError> {
        match self {
            Self::Array(array) => Ok(array),
            _ => Err(SemanticError::ExpectedArray),
        }
    }

    pub const fn is_aggregate(&self) -> bool {
        matches!(self, Self::Struct(_) | Self::Array(_))
    }

    pub const fn should_load(&self) -> bool {
        !self.is_aggregate()
    }

    pub fn cranelift_type(&self, isa: &Arc<dyn TargetIsa>) -> cranelift::prelude::Type {
        match self {
            Self::Primitive(primitive_kind) => primitive_kind.cranelift_type(isa.pointer_type()),
            Self::Struct(_) | Self::Array(_) => isa.pointer_type(),
            Self::Void => todo!("unimplemented - clean up"),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Primitive {
    U8,
    U16,
    U32,
    U64,
    U128,
    I8,
    I16,
    I32,
    I64,
    I128,
    F32,
    F64,
    USize,
}

impl Primitive {
    #[must_use]
    pub const fn cranelift_type(
        &self,
        pointer: cranelift::prelude::Type,
    ) -> cranelift::prelude::Type {
        use cranelift::prelude::*;
        match self {
            Self::U8 | Self::I8 => types::I8,
            Self::U16 | Self::I16 => types::I16,
            Self::F32 => types::F32,
            Self::U32 | Self::I32 => types::I32,
            Self::F64 => types::F64,
            Self::U64 | Self::I64 => types::I64,
            Self::U128 | Self::I128 => types::I128,
            Self::USize => pointer,
        }
    }

    #[must_use]
    pub const fn is_integer(&self) -> bool {
        matches!(
            self,
            Self::I128
                | Self::I64
                | Self::I32
                | Self::I16
                | Self::I8
                | Self::U128
                | Self::U64
                | Self::U32
                | Self::U16
                | Self::U8
                | Self::USize
        )
    }

    #[must_use]
    pub const fn is_signed_integer(&self) -> bool {
        matches!(
            self,
            Self::I128 | Self::I64 | Self::I32 | Self::I16 | Self::I8
        )
    }

    #[must_use]
    pub const fn size(&self, pointer_size: u32) -> u32 {
        match self {
            Self::U8 | Self::I8 => 1,
            Self::U16 | Self::I16 => 2,
            Self::U32 | Self::I32 | Self::F32 => 4,
            Self::U64 | Self::I64 | Self::F64 => 8,
            Self::U128 | Self::I128 => 16,
            Self::USize => pointer_size,
        }
    }
}
