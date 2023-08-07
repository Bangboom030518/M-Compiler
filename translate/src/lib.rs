use cranelift::prelude::{Variable as IRVariable, *};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, Linkage, Module};
use parser::ast::prelude::*;
use std::collections::HashMap;
use std::slice;

/// The basic JIT class.
pub struct JIT {
    /// The function builder context, which is reused across multiple
    /// FunctionBuilder instances.
    builder_context: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen. Cranelift
    /// separates this from `Module` to allow for parallel compilation, with a
    /// context per thread, though this isn't in the simple demo here.
    ctx: codegen::Context,

    /// The data description, which is to data objects what `ctx` is to functions.
    data_description: DataDescription,

    /// The module, with the jit backend, which manages the JIT'd
    /// functions.
    module: JITModule,
}

impl Default for JIT {
    fn default() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {msg}");
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

        let module = JITModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_description: DataDescription::new(),
            module,
        }
    }
}

impl JIT {
    /// Compile a string in the toy language into machine code.
    pub fn compile(
        &mut self,
        Function {
            name,
            parameters,

            return_variable,
            statements,
        }: Function,
    ) -> Result<*const u8, String> {
        // First, parse the string, producing AST nodes.
        // let (name, params, the_return, stmts) =
        //     parser::parse_functions(input).map_err(|e| e.to_string())?;

        // Then, translate the AST nodes into Cranelift IR.
        self.translate(parameters, return_variable, statements)?;

        // Next, declare the function to jit. Functions must be declared
        // before they can be called, or defined.
        let id = self
            .module
            .declare_function(&name.to_string(), Linkage::Export, &self.ctx.func.signature)
            .map_err(|e| e.to_string())?;

        // Define the function to jit. This finishes compilation, although
        // there may be outstanding relocations to perform. Currently, jit
        // cannot finish relocations until all functions to be called are
        // defined. For this toy demo for now, we'll just finalize the
        // function below.
        self.module
            .define_function(id, &mut self.ctx)
            .map_err(|e| e.to_string())?;

        // Now that compilation is finished, we can clear out the context state.
        self.module.clear_context(&mut self.ctx);

        // Finalize the functions which we just defined, which resolves any
        // outstanding relocations (patching in addresses, now that they're
        // available).
        self.module.finalize_definitions().unwrap();

        // We can now retrieve a pointer to the machine code.
        let code = self.module.get_finalized_function(id);

        Ok(code)
    }

    /// Create a zero-initialized data section.
    pub fn create_data(&mut self, name: &str, contents: Vec<u8>) -> Result<&[u8], String> {
        // The steps here are analogous to `compile`, except that data is much
        // simpler than functions.
        self.data_description.define(contents.into_boxed_slice());
        let id = self
            .module
            .declare_data(name, Linkage::Export, true, false)
            .map_err(|e| e.to_string())?;

        self.module
            .define_data(id, &self.data_description)
            .map_err(|e| e.to_string())?;
        self.data_description.clear();
        self.module.finalize_definitions().unwrap();
        let buffer = self.module.get_finalized_data(id);
        Ok(unsafe { slice::from_raw_parts(buffer.0, buffer.1) })
    }

    // Translate from toy-language AST nodes into Cranelift IR.
    fn translate(
        &mut self,
        parameters: Vec<Identifier>,
        the_return: Identifier,
        statements: Vec<Expression>,
    ) -> Result<(), String> {
        // Our toy language currently only supports I64 values, though Cranelift
        // supports other types.
        let int = self.module.target_config().pointer_type();

        for _p in &parameters {
            self.ctx.func.signature.params.push(AbiParam::new(int));
        }

        // Our toy language currently only supports one return value, though
        // Cranelift is designed to support more.
        self.ctx.func.signature.returns.push(AbiParam::new(int));

        // Create the builder to build a function.
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        // Create the entry block, to start emitting code in.
        let entry_block = builder.create_block();

        // Since this is the entry block, add block parameters corresponding to
        // the function's parameters.
        //
        builder.append_block_params_for_function_params(entry_block);

        // Tell the builder to emit code in this block.
        builder.switch_to_block(entry_block);

        // And, tell the builder that this block will have no further
        // predecessors. Since it's the entry block, it won't have any
        // predecessors.
        builder.seal_block(entry_block);

        // The toy language allows variables to be declared implicitly.
        // Walk the AST and declare all implicitly-declared variables.
        let variables =
            declare_variables(int, &mut builder, &parameters, &the_return, &statements, entry_block);

        // Now translate the statements of the function body.
        let mut trans = FunctionTranslator {
            int,
            builder,
            variables,
            module: &mut self.module,
        };
        for expr in statements {
            trans.translate_expression(expr);
        }

        // Set up the return variable of the function. Above, we declared a
        // variable to hold the return value. Here, we just do a use of that
        // variable.
        let return_variable = trans.variables.get(&the_return).unwrap();
        let return_value = trans.builder.use_var(*return_variable);

        // Emit the return instruction.
        trans.builder.ins().return_(&[return_value]);

        // Tell the builder we're done with this function.
        trans.builder.finalize();
        Ok(())
    }
}

/// A collection of state used for translating from toy-language AST nodes
/// into Cranelift IR.
struct FunctionTranslator<'a> {
    int: types::Type,
    builder: FunctionBuilder<'a>,
    variables: HashMap<String, IRVariable>,
    module: &'a mut JITModule,
}

impl<'a> FunctionTranslator<'a> {
    // fn translate_statement(&mut self, statement: Statement) -> {
    //     Statement::
    // }

    /// When you write out instructions in Cranelift, you get back `Value`s. You
    /// can then use these references in other instructions.
    fn translate_expression(&mut self, expression: Expression) -> Value {
        match expression {
            Expression::Literal(literal) => {
                let Literal::Number(Number::Integer(number)) = literal else {
                    todo!("more literals")
                };

                let integer = isize::try_from(number).expect("failed to parse number") as i64;
                self.builder.ins().iconst(self.int, integer)
            }
            Expression::Binary(binary) => {
                let BinaryExpression {
                    left,
                    right,
                    operator,
                } = binary;
                match operator {
                    BinaryOperator::Add => self.builder.ins().iadd(
                        self.translate_expression(*left),
                        self.translate_expression(*right),
                    ),
                    BinaryOperator::Subtract => self.builder.ins().isub(
                        self.translate_expression(*left),
                        self.translate_expression(*right),
                    ),
                    BinaryOperator::Multiply => self.builder.ins().imul(
                        self.translate_expression(*left),
                        self.translate_expression(*right),
                    ),
                    BinaryOperator::Divide => self.builder.ins().udiv(
                        self.translate_expression(*left),
                        self.translate_expression(*right),
                    ),
                    BinaryOperator::Remainder => self.builder.ins().srem(
                        self.translate_expression(*left),
                        self.translate_expression(*right),
                    ),
                    BinaryOperator::Exponent => todo!("allow exponentation"),
                    BinaryOperator::Equal => self.translate_icmp(IntCC::Equal, *left, *right),
                    BinaryOperator::NotEqual => self.translate_icmp(IntCC::NotEqual, *left, *right),
                    BinaryOperator::GreaterThan => {
                        self.translate_icmp(IntCC::SignedGreaterThan, *left, *right)
                    }
                    BinaryOperator::LessThan => {
                        self.translate_icmp(IntCC::SignedLessThan, *left, *right)
                    }
                    BinaryOperator::GreaterThanOrEqual => {
                        self.translate_icmp(IntCC::SignedLessThanOrEqual, *left, *right)
                    }
                    BinaryOperator::LessThanOrEqual => {
                        self.translate_icmp(IntCC::SignedLessThanOrEqual, *left, *right)
                    }
                }
            }
            Expression::Call(call) => {
                let CallExpression {
                    callable,
                    arguments,
                    type_arguments,
                } = call;
                let Expression::Identifier(identifier) = *callable else {
                    todo!("handle calls to non-identifiers");
                };
                self.translate_call(identifier.to_string(), arguments)
            }
            _ => todo!(), // Expr::GlobalDataAddr(name) => self.translate_global_data_addr(name),
                          // Expr::Identifier(name) => {
                          //     // `use_var` is used to read the value of a variable.
                          //     let variable = self.variables.get(&name).expect("variable not defined");
                          //     self.builder.use_var(*variable)
                          // }
                          // Expr::Assign(name, expr) => self.translate_assign(name, *expr),
                          // Expr::IfElse(condition, then_body, else_body) => {
                          //     self.translate_if_else(*condition, then_body, else_body)
                          // }
                          // Expr::WhileLoop(condition, loop_body) => {
                          //     self.translate_while_loop(*condition, loop_body)
                          // }
        }
    }

    fn translate_assign(&mut self, name: String, expr: Expression) -> Value {
        // `def_var` is used to write the value of a variable. Note that
        // variables can have multiple definitions. Cranelift will
        // convert them into SSA form for itself automatically.
        let new_value = self.translate_expression(expr);
        let variable = self.variables.get(&name).unwrap();
        self.builder.def_var(*variable, new_value);
        new_value
    }

    fn translate_icmp(&mut self, cmp: IntCC, lhs: Expression, rhs: Expression) -> Value {
        let lhs = self.translate_expression(lhs);
        let rhs = self.translate_expression(rhs);
        self.builder.ins().icmp(cmp, lhs, rhs)
    }

    fn translate_if_else(
        &mut self,
        condition: Expression,
        then_body: Vec<Expression>,
        else_body: Vec<Expression>,
    ) -> Value {
        let condition_value = self.translate_expression(condition);

        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        // If-else constructs in the toy language have a return value.
        // In traditional SSA form, this would produce a PHI between
        // the then and else bodies. Cranelift uses block parameters,
        // so set up a parameter in the merge block, and we'll pass
        // the return values to it from the branches.
        self.builder.append_block_param(merge_block, self.int);

        // Test the if condition and conditionally branch.
        self.builder
            .ins()
            .brif(condition_value, then_block, &[], else_block, &[]);

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);
        let mut then_return = self.builder.ins().iconst(self.int, 0);
        for expr in then_body {
            then_return = self.translate_expression(expr);
        }

        // Jump to the merge block, passing it the block return value.
        self.builder.ins().jump(merge_block, &[then_return]);

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);
        let mut else_return = self.builder.ins().iconst(self.int, 0);
        for expr in else_body {
            else_return = self.translate_expression(expr);
        }

        // Jump to the merge block, passing it the block return value.
        self.builder.ins().jump(merge_block, &[else_return]);

        // Switch to the merge block for subsequent statements.
        self.builder.switch_to_block(merge_block);

        // We've now seen all the predecessors of the merge block.
        self.builder.seal_block(merge_block);

        // Read the value of the if-else by reading the merge block
        // parameter.
        let phi = self.builder.block_params(merge_block)[0];

        phi
    }

    fn translate_while_loop(&mut self, condition: Expression, loop_body: Vec<Expression>) -> Value {
        let header_block = self.builder.create_block();
        let body_block = self.builder.create_block();
        let exit_block = self.builder.create_block();

        self.builder.ins().jump(header_block, &[]);
        self.builder.switch_to_block(header_block);

        let condition_value = self.translate_expression(condition);
        self.builder
            .ins()
            .brif(condition_value, body_block, &[], exit_block, &[]);

        self.builder.switch_to_block(body_block);
        self.builder.seal_block(body_block);

        for expr in loop_body {
            self.translate_expression(expr);
        }
        self.builder.ins().jump(header_block, &[]);

        self.builder.switch_to_block(exit_block);

        // We've reached the bottom of the loop, so there will be no
        // more backedges to the header to exits to the bottom.
        self.builder.seal_block(header_block);
        self.builder.seal_block(exit_block);

        // Just return 0 for now.
        self.builder.ins().iconst(self.int, 0)
    }

    fn translate_call(&mut self, name: String, args: Vec<Expression>) -> Value {
        let mut sig = self.module.make_signature();

        // Add a parameter for each argument.
        for _arg in &args {
            sig.params.push(AbiParam::new(self.int));
        }

        // For simplicity for now, just make all calls return a single I64.
        sig.returns.push(AbiParam::new(self.int));

        // TODO: Streamline the API here?
        let callee = self
            .module
            .declare_function(&name, Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        let mut arg_values = Vec::new();
        for arg in args {
            arg_values.push(self.translate_expression(arg))
        }
        let call = self.builder.ins().call(local_callee, &arg_values);
        self.builder.inst_results(call)[0]
    }

    fn translate_global_data_addr(&mut self, name: String) -> Value {
        let sym = self
            .module
            .declare_data(&name, Linkage::Export, true, false)
            .expect("problem declaring data object");
        let local_id = self.module.declare_data_in_func(sym, self.builder.func);

        let pointer = self.module.target_config().pointer_type();
        self.builder.ins().symbol_value(pointer, local_id)
    }
}

fn declare_variables(
    int: types::Type,
    builder: &mut FunctionBuilder,
    params: &[Identifier],
    the_return: Identifier,
    stmts: &[Expression],
    entry_block: Block,
) -> HashMap<Identifier, IRVariable> {
    let mut variables = HashMap::new();
    let mut index = 0;

    for (i, name) in params.iter().enumerate() {
        // up param variables.
        let val = builder.block_params(entry_block)[i];
        let var = declare_variable(int, builder, &mut variables, &mut index, name);
        builder.def_var(var, val);
    }
    let zero = builder.ins().iconst(int, 0);
    let return_variable = declare_variable(int, builder, &mut variables, &mut index, the_return);
    builder.def_var(return_variable, zero);
    for expr in stmts {
        declare_variables_in_stmt(int, builder, &mut variables, &mut index, expr);
    }

    variables
}

/// Recursively descend through the AST, translating all implicit
/// variable declarations.
fn declare_variables_in_stmt(
    int: types::Type,
    builder: &mut FunctionBuilder,
    variables: &mut HashMap<String, IRVariable>,
    index: &mut usize,
    expr: &Expression,
) {
    // TODO: uncomment
    /*
    match *expr {
        Expr::Assign(ref name, _) => {
            declare_variable(int, builder, variables, index, name);
        }
        Expr::IfElse(ref _condition, ref then_body, ref else_body) => {
            for stmt in then_body {
                declare_variables_in_stmt(int, builder, variables, index, stmt);
            }
            for stmt in else_body {
                declare_variables_in_stmt(int, builder, variables, index, stmt);
            }
        }
        Expr::WhileLoop(ref _condition, ref loop_body) => {
            for stmt in loop_body {
                declare_variables_in_stmt(int, builder, variables, index, stmt);
            }
        }
        _ => (),
    }
    */
}

/// Declare a single variable declaration.
fn declare_variable(
    int: types::Type,
    builder: &mut FunctionBuilder,
    variables: &mut HashMap<Identifier, IRVariable>,
    index: &mut usize,
    name: &Identifier,
) -> IRVariable {
    let var = IRVariable::new(*index);
    if !variables.contains_key(name) {
        variables.insert(name.clone().into(), var);
        builder.declare_var(var, int);
        *index += 1;
    }
    var
}
