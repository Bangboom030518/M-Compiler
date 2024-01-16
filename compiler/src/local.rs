use crate::top_level_resolution::{self, Type};
use crate::SemanticError;
use cranelift::prelude::*;
use cranelift_module::Module;
use parser::expression::control_flow::If;
use parser::expression::{Call, IntrinsicCall, IntrinsicOperator, UnaryOperator};
use parser::prelude::Literal;
use parser::Expression;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Value {
    Cranelift(cranelift::prelude::Value, top_level_resolution::Id),
    UnknownSignedIntegerConst(i128),
    UnknownIntegerConst(u128),
    UnknownFloatConst(f64),
    Binary(Box<Value>, Box<Value>, IntrinsicOperator),
    If {
        condition: Box<Value>,
        then_branch: Vec<parser::Statement>,
        else_branch: Vec<parser::Statement>,
    },
}

impl Value {
    /// remove unknowns
    fn unwrap(
        self,
        type_id: top_level_resolution::Id,
        builder: &mut FunctionBuilder<impl Module>,
    ) -> Result<cranelift::prelude::Value, SemanticError> {
        let r#type = builder.declarations.get_type(type_id)?;
        // TODO: support unsigned integers
        match self {
            Self::UnknownIntegerConst(int) => {
                let r#type = match r#type {
                    Type::I8 | Type::U8 => types::I8,
                    Type::I16 | Type::U16 => types::I16,
                    Type::I32 | Type::U32 => types::I32,
                    Type::I64 | Type::U64 => types::I64,
                    Type::I128 | Type::U128 => todo!("chonky intz"),
                    _ => return Err(SemanticError::UnexpectedNumberLiteral),
                };
                Ok(builder.builder.ins().iconst(r#type, i64::try_from(int)?))
            }
            Self::UnknownSignedIntegerConst(int) => {
                let r#type = match r#type {
                    Type::I8 => types::I8,
                    Type::I16 => types::I16,
                    Type::I32 => types::I32,
                    Type::I64 => types::I64,
                    Type::I128 => todo!("chonky intz"),
                    _ => return Err(SemanticError::UnexpectedNumberLiteral),
                };
                Ok(builder.builder.ins().iconst(r#type, i64::try_from(int)?))
            }
            Self::UnknownFloatConst(float) => match r#type {
                // TODO: as
                Type::F32 => Ok(builder.builder.ins().f32const(Ieee32::from(float as f32))),
                Type::F64 => Ok(builder.builder.ins().f64const(Ieee64::from(float))),
                _ => Err(SemanticError::UnexpectedNumberLiteral),
            },
            Self::Cranelift(value, value_type) => {
                if value_type == type_id {
                    Ok(value)
                } else {
                    Err(SemanticError::MismatchedTypes)
                }
            }
            Self::Binary(left, right, operator) => {
                let type_id = left
                    .r#type()
                    .or_else(|| right.r#type())
                    .ok_or(SemanticError::UnknownType)?;
                let left = left.unwrap(type_id, builder)?;
                let right = right.unwrap(type_id, builder)?;
                let r#type = builder.declarations.get_type(type_id)?;

                match operator {
                    IntrinsicOperator::IAdd => Ok(builder.builder.ins().iadd(left, right)),
                    operator => {
                        if r#type.is_signed_integer() {
                            let cc = match operator {
                                IntrinsicOperator::Eq => IntCC::Equal,
                                IntrinsicOperator::Ne => IntCC::NotEqual,
                                IntrinsicOperator::Gt => IntCC::SignedGreaterThan,
                                IntrinsicOperator::Gte => IntCC::SignedGreaterThanOrEqual,
                                IntrinsicOperator::Lt => IntCC::SignedLessThan,
                                IntrinsicOperator::Lte => IntCC::SignedLessThanOrEqual,
                                // TODO: `unreachable!()`
                                IntrinsicOperator::IAdd => unreachable!(),
                            };
                            Ok(builder.builder.ins().icmp(cc, left, right))
                        } else if r#type.is_integer() {
                            let cc = match operator {
                                IntrinsicOperator::Eq => IntCC::Equal,
                                IntrinsicOperator::Ne => IntCC::NotEqual,
                                IntrinsicOperator::Gt => IntCC::UnsignedGreaterThan,
                                IntrinsicOperator::Gte => IntCC::UnsignedGreaterThanOrEqual,
                                IntrinsicOperator::Lt => IntCC::UnsignedLessThan,
                                IntrinsicOperator::Lte => IntCC::UnsignedLessThanOrEqual,
                                // TODO: `unreachable!()`
                                IntrinsicOperator::IAdd => unreachable!(),
                            };
                            Ok(builder.builder.ins().icmp(cc, left, right))
                        } else {
                            let cc = match operator {
                                IntrinsicOperator::Eq => FloatCC::Equal,
                                IntrinsicOperator::Ne => FloatCC::NotEqual,
                                IntrinsicOperator::Gt => todo!(),
                                IntrinsicOperator::Gte => todo!(),
                                IntrinsicOperator::Lt => todo!(),
                                IntrinsicOperator::Lte => todo!(),
                                // TODO: `unreachable!()`
                                IntrinsicOperator::IAdd => unreachable!(),
                            };
                            Ok(builder.builder.ins().fcmp(cc, left, right))
                        }
                    }
                }
            }
            Self::If {
                condition,
                mut then_branch,
                mut else_branch,
            } => {
                let condition_type = condition.r#type().ok_or(SemanticError::UnknownType)?;
                let condition = condition.unwrap(condition_type, builder)?;

                let then_block = builder.builder.create_block();
                let else_block = builder.builder.create_block();
                let merge_block = builder.builder.create_block();

                builder
                    .builder
                    .append_block_param(merge_block, r#type.cranelift_type());

                builder
                    .builder
                    .ins()
                    .brif(condition, then_block, &[], else_block, &[]);

                builder.builder.switch_to_block(then_block);
                builder.builder.seal_block(then_block);
                let then_return = then_branch.pop();
                for statement in then_branch {
                    builder.handle_statement(statement)?;
                }

                if let Some(parser::Statement::Expression(then_return)) = then_return {
                    let value = builder.expression(then_return, r#type)?;
                    builder.builder.ins().jump(merge_block, &[]);
                } else {
                    then_return.map(|then_return| builder.handle_statement(then_return));
                    builder.builder.ins().jump(merge_block, &[]);
                }

                builder.builder.switch_to_block(then_block);
                builder.builder.seal_block(then_block);
                if let Some(false_branch) = else_branch {
                    for statement in false_branch {
                        builder.handle_statement(statement)?;
                    }
                }
                builder.builder.ins().jump(merge_block, &[]);

                builder.builder.switch_to_block(merge_block);
                builder.builder.seal_block(merge_block);

                Ok(Value::Cranelift(
                    builder.builder.block_params(merge_block)[0],
                    r#type.unwrap_or_else(|| todo!()),
                ))
            }
        }
    }

    pub const fn r#type(&self) -> Option<top_level_resolution::Id> {
        let Self::Cranelift(_, r#type) = self else {
            return None;
        };
        Some(*r#type)
    }

    // pub fn unwrap(
    //     self,
    //     type_id: top_level_resolution::Id,
    //     builder: &mut FunctionBuilder<impl Module>,
    // ) -> Result<cranelift::prelude::Value, SemanticError> {
    //     match self {
    //         Self::UnknownSignedIntegerConst(_)
    //         | Self::UnknownIntegerConst(_)
    //         | Self::UnknownFloatConst(_) => Err(SemanticError::UnknownType),
    //         Self::Cranelift(value, value_type) => {
    //             if value_type != type_id {
    //                 Err(SemanticError::MismatchedTypes)
    //             } else {
    //                 Ok(value)
    //             }
    //         }
    //         Self::IAdd(left, right) => todo!(),
    //     }
    // }
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
                if r#type == value.r#type() {
                    return Err(SemanticError::InvalidAssignment);
                };
                // TODO: inference
                let value = value.unwrap(r#type.unwrap(), self)?;

                self.builder.def_var(variable, value);
            }
            parser::Statement::Let(ident, expression) => {
                let value = self.expression(expression, None)?;
                let variable = self.create_variable();
                let m_type = match value.r#type() {
                    Some(r#type) => Some(self.declarations.get_type(r#type)?.clone()),
                    None => None,
                };

                let cranelift_type =
                    m_type.map_or_else(|| todo!(), |r#type| r#type.cranelift_type());

                self.builder.declare_var(variable, cranelift_type);

                self.names.insert(ident, (variable, value.r#type()));
                // TODO: lazily promote
                // TODO: type inference
                // TODO: `.unwrap()`
                let r#type = value.r#type().unwrap();
                let value = value.unwrap(r#type, self)?;

                self.builder.def_var(variable, value);
            }
            parser::Statement::Expression(expression) => {
                if let Expression::Return(expression) = expression {
                    let value = self
                        .expression(*expression, Some(self.r#return))?
                        .unwrap(self.r#return, self)?;

                    self.builder.ins().return_(&[value]);
                } else {
                    self.expression(expression, None)?;
                }
            }
        };

        Ok(())
    }

    fn integer(
        &mut self,
        int: u128,
        type_id: Option<top_level_resolution::Id>,
    ) -> Result<Value, SemanticError> {
        let Some(r#type) = type_id
            .map(|r#type| self.declarations.get_type(r#type))
            .transpose()?
        else {
            return Ok(Value::UnknownIntegerConst(int));
        };

        if !r#type.is_integer() {
            return Err(SemanticError::UnexpectedNumberLiteral);
        };

        // TODO: `.unwrap()`
        // TODO: chonky intz
        Ok(Value::Cranelift(
            self.builder
                .ins()
                .iconst(r#type.cranelift_type(), i64::try_from(int)?),
            type_id.unwrap(),
        ))
    }

    fn float(
        &mut self,
        float: f64,
        type_id: Option<top_level_resolution::Id>,
    ) -> Result<Value, SemanticError> {
        // TODO: `.unwrap()`
        // TODO: refactor
        let Some(r#type) = type_id
            .map(|r#type| self.declarations.get_type(r#type))
            .transpose()?
        else {
            return Ok(Value::UnknownFloatConst(float));
        };

        let value = match r#type {
            // TODO: `as`
            Type::F32 => Value::Cranelift(
                self.builder.ins().f32const(Ieee32::from(float as f32)),
                type_id.unwrap(),
            ),
            Type::F64 => Value::Cranelift(
                self.builder.ins().f64const(Ieee64::from(float)),
                type_id.unwrap(),
            ),
            _ => return Err(SemanticError::UnexpectedNumberLiteral),
        };
        Ok(value)
    }

    fn literal(
        &mut self,
        literal: Literal,
        r#type: Option<top_level_resolution::Id>,
    ) -> Result<Value, SemanticError> {
        match literal {
            Literal::Integer(integer) => Ok(self.integer(integer, r#type)?),
            Literal::Float(float) => Ok(self.float(float, r#type)?),
            _ => todo!(),
        }
    }

    fn intrinsic_call(&mut self, intrinsic: IntrinsicCall) -> Result<Value, SemanticError> {
        match intrinsic {
            IntrinsicCall::Binary(left, right, operator) => {
                let mut left = self.expression(*left, None)?;
                let right = self.expression(*right, left.r#type())?;
                if let Some(r#type) = right.r#type() {
                    // TODO: `.clone()`
                    left = Value::Cranelift(left.unwrap(r#type, self)?, r#type);
                }
                Ok(Value::Binary(Box::new(left), Box::new(right), operator))
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
                let value = self.expression(*expression, Some(r#type))?;
                Ok(Value::Cranelift(value.unwrap(r#type, self)?, r#type))
            }
        }
    }

    fn unary_prefix(
        &mut self,
        operator: UnaryOperator,
        expression: Expression,
        type_id: Option<top_level_resolution::Id>,
    ) -> Result<Value, SemanticError> {
        match (operator, expression) {
            (UnaryOperator::Minus, Expression::Literal(Literal::Integer(integer))) => {
                let Some(r#type) = type_id
                    .map(|r#type| self.declarations.get_type(r#type))
                    .transpose()?
                else {
                    return Ok(Value::UnknownSignedIntegerConst(-i128::try_from(integer)?));
                };

                if !r#type.is_signed_integer() {
                    return Err(SemanticError::UnexpectedNumberLiteral);
                };

                Ok(Value::Cranelift(
                    self.builder
                        .ins()
                        .iconst(r#type.cranelift_type(), -i64::try_from(integer)?),
                    type_id.unwrap(),
                ))
            }
            (UnaryOperator::Minus, Expression::Literal(Literal::Float(float))) => {
                self.float(-float, type_id)
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
    ) -> Result<Value, SemanticError> {
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
                    .and_then(|value| value.unwrap(*r#type, self))
            })
            .collect::<Result<Vec<_>, _>>()?;

        let func_ref = self
            .module
            .declare_func_in_func(function.id, self.builder.func);
        let call = self.builder.ins().call(func_ref, arguments.as_slice());
        let value = self.builder.inst_results(call)[0];

        Ok(Value::Cranelift(value, function.r#return))
    }

    fn expression(
        &mut self,
        expression: Expression,
        r#type: Option<top_level_resolution::Id>,
    ) -> Result<Value, SemanticError> {
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
                Ok(Value::Cranelift(
                    self.builder.use_var(variable.0),
                    variable.1.unwrap_or_else(|| todo!()),
                ))
            }
            Expression::Call(call) => {
                let value = self.call(call)?;
                if let r#type @ Some(_) = r#type {
                    if r#type != value.r#type() {
                        return Err(SemanticError::MismatchedTypes);
                    }
                };
                Ok(value)
            }
            Expression::If(If {
                condition,
                true_branch,
                false_branch,
            }) => Ok(Value::If {
                condition: Box::new(self.expression(*condition, None)?),
                then_branch: true_branch,
                else_branch: false_branch.into_iter().flatten().collect(),
            }),
            _ => todo!(),
        }
    }
}
