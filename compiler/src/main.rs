#![warn(clippy::pedantic, clippy::nursery)]

mod local;
mod type_resolution;

use cranelift::codegen::ir::InstBuilder;
use cranelift::prelude::*;
use cranelift_module::Module;
use parser::{top_level::DeclarationKind, Expression};
use std::collections::HashMap;

fn main() {
    let file = parser::parse_file(include_str!("../../input.m")).unwrap();
    let root = file.root;
    let mut type_store = type_resolution::TypeStore {
        types: Vec::new(),
        scopes: HashMap::new(),
        file_cache: file.cache,
    };

    let mut flag_builder = settings::builder();
    flag_builder.set("use_colocated_libcalls", "false").unwrap();
    flag_builder.set("is_pic", "false").unwrap();
    let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
        panic!("host machine is not supported: {}", msg);
    });
    let isa = isa_builder
        .finish(settings::Flags::new(flag_builder))
        .unwrap();
    let builder =
        cranelift_jit::JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

    let module = cranelift_jit::JITModule::new(builder);
    let context = module.make_context();
    let function_builder_context = FunctionBuilderContext::new();

    type_resolution::TypeScope::append_new(&mut type_store, file.root).unwrap();
    for declaration in type_store.file_cache[root].declarations.values() {
        let DeclarationKind::Function(function) = declaration else {
            continue;
        };

        let Some((return_statement, statements)) = function.body.split_last() else {
            todo!("handle empty body")
        };

        // TODO: `.clone()`?
        let return_statement = if let parser::Statement::Expression(expression) = return_statement {
            parser::Statement::Expression(Expression::Return(Box::new(expression.clone())))
        } else {
            return_statement.clone()
        };

        let mut local_scope = local::Scope::new();
        // TODO: params!
        let value_builder =
            local::ValueBuilder::new(&type_store, &mut local_scope, root, function.parameters.iter().map(|parser::top_level::Parameter(parameter)| {
                (parameter.name, parameter.type)
            }));
        // TODO: not root
        let mut value_builder = value_builder.with_return_type(
            type_store
                .lookup(
                    match function.return_type.as_ref().unwrap() {
                        parser::Type::Identifier(ident) => ident,
                    },
                    root,
                )
                .unwrap(),
        );

        let statements = statements
            .iter()
            .chain(std::iter::once(&return_statement))
            .map(|statement| value_builder.handle_statement(statement))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();

        // Then, translate the AST nodes into Cranelift IR.
        // Our toy language currently only supports I64 values, though Cranelift
        // supports other types.
        let int = module.target_config().pointer_type();
        
        for _p in &params {
            context.func.signature.params.push(AbiParam::new(int));
        }

        // Our toy language currently only supports one return value, though
        // Cranelift is designed to support more.
        context.func.signature.returns.push(AbiParam::new(int));

        // Create the builder to build a function.
        let mut builder = FunctionBuilder::new(&mut context.func, &mut function_builder_context);

        // Create the entry block, to start emitting code in.
        let entry_block = builder.create_block();

        // Since this is the entry block, add block parameters corresponding to
        // the function's parameters.
        //
        // TODO: Streamline the API here.
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
            declare_variables(int, &mut builder, &params, &the_return, &stmts, entry_block);

        // Now translate the statements of the function body.
        let mut trans = FunctionTranslator {
            int,
            builder,
            variables,
            module: &mut self.module,
        };
        for expr in stmts {
            trans.translate_expr(expr);
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

        // Next, declare the function to jit. Functions must be declared
        // before they can be called, or defined.
        //
        // we have a version of `declare_function` that automatically declares
        // the function?
        let id = self
            .module
            .declare_function(&name, Linkage::Export, &self.ctx.func.signature)
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
    }

    // Finalize the functions which we just defined, which resolves any
    // outstanding relocations (patching in addresses, now that they're
    // available).
    self.module.finalize_definitions().unwrap();

    // We can now retrieve a pointer to the machine code.
    let code = self.module.get_finalized_function(id);

    Ok(code)
}
