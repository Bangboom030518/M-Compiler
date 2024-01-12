use crate::top_level_resolution::{self, Type};
use crate::SemanticError;
use cranelift::prelude::*;
use cranelift_module::Module;
use parser::expression::{Call, IntrinsicCall, UnaryOperator};
use parser::prelude::Literal;
use parser::Expression;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Value {
    Cranelift(cranelift::prelude::Value, top_level_resolution::Id),
    UnknownSignedIntegerConst(i128),
    UnknownIntegerConst(u128),
    UnknownFloatConst(f64),
}

impl Value {
    /// remove unknowns
    fn promote(
        self,
        type_id: top_level_resolution::Id,
        builder: &mut FunctionBuilder<impl Module>,
    ) -> Result<cranelift::prelude::Value, SemanticError> {
        let r#type = builder.declarations.get_type(type_id)?;
        // TODO: support unsigned integers
        match self {
            Self::UnknownIntegerConst(int) => match r#type {
                Type::U8 => todo!(),
                Type::U16 => todo!(),
                Type::U32 => todo!(),
                Type::U64 => todo!(),
                Type::U128 => todo!(),
                Type::I8 => Ok(builder.builder.ins().iconst(types::I8, i64::try_from(int)?)),
                Type::I16 => Ok(builder
                    .builder
                    .ins()
                    .iconst(types::I16, i64::try_from(int)?)),
                Type::I32 => Ok(builder
                    .builder
                    .ins()
                    .iconst(types::I32, i64::try_from(int)?)),
                Type::I64 => Ok(builder
                    .builder
                    .ins()
                    .iconst(types::I64, i64::try_from(int)?)),
                Type::I128 => todo!(),
                _ => Err(SemanticError::UnexpectedIntegerLiteral),
            },
            Self::UnknownSignedIntegerConst(int) => match r#type {
                Type::I8 => Ok(builder.builder.ins().iconst(types::I8, i64::try_from(int)?)),
                Type::I16 => Ok(builder
                    .builder
                    .ins()
                    .iconst(types::I16, i64::try_from(int)?)),
                Type::I32 => Ok(builder
                    .builder
                    .ins()
                    .iconst(types::I32, i64::try_from(int)?)),
                Type::I64 => Ok(builder
                    .builder
                    .ins()
                    .iconst(types::I64, i64::try_from(int)?)),
                Type::I128 => todo!(),
                _ => Err(SemanticError::UnexpectedIntegerLiteral),
            },
            Self::UnknownFloatConst(float) => match r#type {
                // TODO: as
                Type::F32 => Ok(builder.builder.ins().f32const(Ieee32::from(float as f32))),
                Type::F64 => Ok(builder.builder.ins().f64const(Ieee64::from(float))),
                _ => Err(SemanticError::UnexpectedIntegerLiteral),
            },
            Self::Cranelift(value, value_type) => {
                if value_type != type_id {
                    Err(SemanticError::MismatchedTypes)
                } else {
                    Ok(value)
                }
            }
        }
    }

    pub fn cranelift_value(
        self,
        type_id: top_level_resolution::Id,
    ) -> Result<cranelift::prelude::Value, SemanticError> {
        match self {
            Self::UnknownSignedIntegerConst(_)
            | Self::UnknownIntegerConst(_)
            | Self::UnknownFloatConst(_) => Err(SemanticError::UnknownType),
            Self::Cranelift(value, value_type) => {
                if value_type != type_id {
                    Err(SemanticError::MismatchedTypes)
                } else {
                    Ok(value)
                }
            }
        }
    }
}

pub struct FunctionBuilder<'a, M> {
    pub declarations: &'a top_level_resolution::TopLevelDeclarations,
    pub module: &'a mut M,
    pub scope: parser::scope::Id,
    pub r#return: top_level_resolution::Id,
    pub names: HashMap<parser::Ident, (Variable, Option<top_level_resolution::Id>)>,
    pub builder: cranelift::prelude::FunctionBuilder<'a>,
    pub new_variable_index: usize,
}

impl<'a, M> FunctionBuilder<'a, M>
where
    M: Module,
{
    fn create_variable(&mut self) -> Variable {
        let variable = Variable::new(self.new_variable_index);
        self.new_variable_index += 1;
        variable
    }

    pub fn handle_statement(&mut self, statement: parser::Statement) -> Result<(), SemanticError> {
        match statement {
            parser::Statement::Assignment(parser::Assignment(name, expression)) => {
                let (variable, r#type) = *self
                    .names
                    .get(&name)
                    .ok_or(SemanticError::DeclarationNotFound)?;

                let value = self.expression(expression, r#type)?;
                if r#type == value.1 {
                    return Err(SemanticError::InvalidAssignment);
                };
                // TODO: inference
                let value = value
                    .0
                    .cranelift_value(r#type.unwrap())
                    .unwrap_or_else(|error| todo!("handle me :( {error}"));

                self.builder.def_var(variable, value);
            }
            parser::Statement::Let(ident, expression) => {
                let value = self.expression(expression, None)?;
                let variable = self.create_variable();
                let m_type = match value.1 {
                    Some(r#type) => Some(self.declarations.get_type(r#type)?.clone()),
                    None => None,
                };

                let cranelift_type = m_type.map_or(cranelift::prelude::types::INVALID, |r#type| {
                    r#type.cranelift_type()
                });

                self.builder.declare_var(variable, cranelift_type);

                self.names.insert(ident, (variable, value.1));
                // TODO: lazily promote
                // TODO: type inference
                let value = value
                    .0
                    .cranelift_value(value.1.unwrap())
                    .unwrap_or_else(|error| todo!("handle me :( {error}"));

                self.builder.def_var(variable, value);
            }
            parser::Statement::Expression(expression) => {
                if let Expression::Return(expression) = expression {
                    let value = self
                        .expression(*expression, Some(self.r#return))?
                        .0
                        .cranelift_value(&mut self.builder)
                        .unwrap_or_else(|error| todo!("handle me :( {error}"));

                    self.builder.ins().return_(&[value]);
                } else {
                    self.expression(expression, None)?
                        .0
                        .cranelift_value(&mut self.builder)
                        .unwrap_or_else(|error| todo!("handle me :) {error}"));
                }
            }
        };

        Ok(())
    }

    fn integer(
        &mut self,
        integer: u128,
        r#type: Option<top_level_resolution::Id>,
    ) -> Result<(Value, Option<top_level_resolution::Id>), SemanticError> {
        let value = match self.r#type(r#type)? {
            Some(Type::U8) => todo!(),
            Some(Type::U16) => todo!(),
            Some(Type::U32) => todo!(),
            Some(Type::U64) => todo!(),
            Some(Type::U128) => todo!(),
            Some(Type::I8) => Value::Cranelift(
                self.builder
                    .ins()
                    .iconst(types::I8, i64::try_from(integer)?),
            ),
            Some(Type::I16) => Value::Cranelift(
                self.builder
                    .ins()
                    .iconst(types::I16, i64::try_from(integer)?),
            ),
            Some(Type::I32) => Value::Cranelift(
                self.builder
                    .ins()
                    .iconst(types::I32, i64::try_from(integer)?),
            ),
            Some(Type::I64) => Value::Cranelift(
                self.builder
                    .ins()
                    .iconst(types::I64, i64::try_from(integer)?),
            ),
            Some(Type::I128) => todo!(),
            None => Value::UnknownIntegerConst(integer),
            _ => return Err(SemanticError::UnexpectedIntegerLiteral),
        };

        Ok((value, r#type))
    }

    fn float(
        &mut self,
        float: f64,
        r#type: Option<top_level_resolution::Id>,
    ) -> Result<(Value, Option<top_level_resolution::Id>), SemanticError> {
        let value = match self.r#type(r#type)? {
            // TODO: `as`
            Some(Type::F32) => {
                Value::Cranelift(self.builder.ins().f32const(Ieee32::from(float as f32)))
            }
            Some(Type::F64) => Value::Cranelift(self.builder.ins().f64const(Ieee64::from(float))),
            None => Value::UnknownFloatConst(float),
            Some(_) => return Err(SemanticError::UnexpectedIntegerLiteral),
        };
        Ok((value, r#type))
    }

    fn literal(
        &mut self,
        literal: Literal,
        r#type: Option<top_level_resolution::Id>,
    ) -> Result<(Value, Option<top_level_resolution::Id>), SemanticError> {
        match literal {
            Literal::Integer(integer) => Ok(self.integer(integer, r#type)?),
            Literal::Float(float) => Ok(self.float(float, r#type)?),
            _ => todo!(),
        }
    }

    fn intrinsic_call(
        &mut self,
        intrinsic: IntrinsicCall,
    ) -> Result<(Value, Option<top_level_resolution::Id>), SemanticError> {
        match intrinsic {
            IntrinsicCall::IAdd(left, right) => {
                let mut left = self.expression(*left, None)?;
                let right = self.expression(*right, left.1)?;
                if let Some(r#type) = right.1 {
                    // TODO: `.clone()`
                    left = (
                        Value::Cranelift(left.0.promote(
                            &self.r#type(Some(r#type))?.expect("TODO: fixme").clone(),
                            &mut self.builder,
                        )?),
                        Some(r#type),
                    );
                }
                let left_value = left.0.cranelift_value(&mut self.builder)?;
                let right_value = right.0.cranelift_value(&mut self.builder)?;
                Ok((
                    Value::Cranelift(self.builder.ins().iadd(left_value, right_value)),
                    left.1,
                ))
            }
            IntrinsicCall::AssertType(expression, r#type) => {
                let r#type = self
                    .declarations
                    .lookup(
                        &match r#type {
                            parser::Type::Identifier(identifier) => identifier,
                        },
                        self.scope,
                    )
                    .ok_or(SemanticError::DeclarationNotFound)?;
                Ok(self.expression(*expression, Some(r#type))?)
            }
        }
    }

    fn unary_prefix(
        &mut self,
        operator: UnaryOperator,
        expression: Expression,
        r#type: Option<top_level_resolution::Id>,
    ) -> Result<(Value, Option<top_level_resolution::Id>), SemanticError> {
        match (operator, expression) {
            (UnaryOperator::Minus, Expression::Literal(Literal::Integer(integer))) => {
                let value = match self.r#type(r#type)? {
                    Some(Type::I8) => Value::Cranelift(
                        self.builder
                            .ins()
                            .iconst(types::I8, -i64::try_from(integer)?),
                    ),
                    Some(Type::I16) => Value::Cranelift(
                        self.builder
                            .ins()
                            .iconst(types::I16, -i64::try_from(integer)?),
                    ),
                    Some(Type::I32) => Value::Cranelift(
                        self.builder
                            .ins()
                            .iconst(types::I32, -i64::try_from(integer)?),
                    ),
                    Some(Type::I64) => Value::Cranelift(
                        self.builder
                            .ins()
                            .iconst(types::I64, -i64::try_from(integer)?),
                    ),
                    None => Value::UnknownSignedIntegerConst(-i128::try_from(integer)?),
                    Some(_) => return Err(SemanticError::UnexpectedIntegerLiteral),
                };
                Ok((value, r#type))
            }
            (UnaryOperator::Minus, Expression::Literal(Literal::Float(float))) => {
                self.float(-float, r#type)
            }
            _ => todo!(),
        }
    }

    fn call(
        &mut self,
        Call {
            callable,
            arguments,
            ..
        }: Call,
    ) -> Result<(Value, top_level_resolution::Id), SemanticError> {
        // TODO: what about if not ident
        let callable = match callable.as_ref() {
            Expression::Identifier(ident) => ident,
            _ => todo!(),
        };
        let function = self
            .declarations
            .lookup(callable, self.scope)
            .ok_or(SemanticError::DeclarationNotFound)?;
        let function = self.declarations.get_function(function)?;

        if arguments.len() != function.parameters.len() {
            return Err(SemanticError::InvalidNumberOfArguments);
        }

        // TODO: type passdown
        let arguments = arguments
            .into_iter()
            .zip(&function.parameters)
            .map(|(expression, (_, r#type))| {
                self.expression(expression, Some(*r#type))
                    .and_then(|(value, _)| value.cranelift_value(&mut self.builder))
            })
            .collect::<Result<Vec<_>, _>>()?;

        let func_ref = self
            .module
            .declare_func_in_func(function.id, self.builder.func);
        let call = self.builder.ins().call(func_ref, arguments.as_slice());
        let value = self.builder.inst_results(call)[0];

        Ok((Value::Cranelift(value), function.r#return))
    }

    fn expression(
        &mut self,
        expression: Expression,
        r#type: Option<top_level_resolution::Id>,
    ) -> Result<(Value, Option<top_level_resolution::Id>), SemanticError> {
        match expression {
            Expression::Literal(literal) => self.literal(literal, r#type),
            Expression::UnaryPrefix(operator, expression) => {
                self.unary_prefix(operator, *expression, r#type)
            }
            Expression::IntrinsicCall(intrinsic) => self.intrinsic_call(intrinsic),
            Expression::Return(expression) => self.expression(*expression, Some(self.r#return)),
            Expression::Identifier(ident) => {
                let variable = *self
                    .names
                    .get(&ident)
                    .ok_or(SemanticError::DeclarationNotFound)?;
                if let (Some(r#type), Some(variable)) = (r#type, variable.1) {
                    if r#type != variable {
                        return Err(SemanticError::MismatchedTypes);
                    }
                };
                Ok((
                    Value::Cranelift(self.builder.use_var(variable.0)),
                    variable.1,
                ))
            }
            Expression::Call(call) => {
                let result = self.call(call)?;
                if let Some(r#type) = r#type {
                    if r#type != result.1 {
                        return Err(SemanticError::MismatchedTypes);
                    }
                };
                Ok((result.0, Some(result.1)))
            }
            _ => todo!(),
        }
    }
}
