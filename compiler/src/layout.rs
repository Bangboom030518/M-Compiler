use crate::declarations;
use crate::declarations::Declarations;
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::codegen::isa::TargetIsa;
use parser::Ident;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Field {
    pub type_id: declarations::Id,
    pub offset: Offset32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Struct {
    pub fields: HashMap<Ident, Field>,
    pub size: u32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Layout {
    Struct(Struct),
    Primitive(Primitive),
}

impl Layout {
    pub fn size(&self, isa: &Arc<dyn TargetIsa>) -> u32 {
        match self {
            Self::Primitive(primitive) => primitive.size(u32::from(isa.pointer_bytes())),
            Self::Struct(Struct { size, .. }) => *size,
        }
    }

    pub const fn is_aggregate(&self) -> bool {
        matches!(self, Self::Struct(_))
    }

    pub const fn should_load(&self) -> bool {
        match self {
            Self::Struct(_) => false,
            Self::Primitive(primitive) => !primitive.is_pointer(),
        }
    }

    pub fn cranelift_type(&self, isa: &Arc<dyn TargetIsa>) -> cranelift::prelude::Type {
        match self {
            Self::Primitive(primitive_kind) => primitive_kind.cranelift_type(isa.pointer_type()),
            Self::Struct(_) => isa.pointer_type(),
        }
    }

    pub fn deref_pointers<'a>(&'a self, declarations: &'a Declarations) -> &'a Self {
        match self {
            Self::Primitive(Primitive::MutablePointer(inner_type)) => declarations
                .get_layout(*inner_type)
                .deref_pointers(declarations),
            _ => self,
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
    MutablePointer(declarations::Id),
    MutableSlice(declarations::Id),
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
            Self::MutablePointer(_) | Self::MutableSlice(_) | Self::USize => pointer,
        }
    }

    #[must_use]
    pub const fn is_pointer(&self) -> bool {
        matches!(self, Self::MutablePointer(_) | Self::MutableSlice(_))
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
            Self::MutablePointer(_) | Self::MutableSlice(_) | Self::USize => pointer_size,
        }
    }
}
