#![warn(clippy::pedantic, clippy::nursery)]
#![feature(iter_collect_into)]

mod local;
mod type_resolution;

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

    let mut module = cranelift_jit::JITModule::new(builder);
    let mut context = module.make_context();
    let mut function_builder_context = FunctionBuilderContext::new();

    type_resolution::TypeScope::append_new(&mut type_store, file.root).unwrap();
    for (name, declaration) in type_store.file_cache[root].declarations.clone() {
        // TODO: not root
        let scope = root;
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


        let parameters: Vec<(parser::prelude::Ident, type_resolution::Id)> = function
            .parameters
            .into_iter()
            .map(
                |parser::top_level::Parameter(parser::top_level::TypeBinding { r#type, name })| {
                    let r#type = r#type
                        .and_then(|r#type| {
                            let parser::Type::Identifier(ident) = r#type;
                            type_store.lookup(&ident, scope)
                        })
                        .unwrap_or_else(|| todo!("semantic error!"));
                    (name, r#type)
                },
            )
            .collect();

        for (param, value) in parameters.iter().zip(builder.block_params(entry_block)) {

            builder.def_var(, value)
        }

        let mut value_builder =
            local::FunctionBuilder::new(&type_store, root, parameters, type_store
                .lookup(
                    match function.return_type.as_ref().unwrap() {
                        parser::Type::Identifier(ident) => ident,
                    },
                    scope,
                )
                .unwrap())
                ;

        let statements = statements
            .iter()
            .chain(std::iter::once(&return_statement))
            .map(|statement| value_builder.handle_statement(statement))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();

        context.func.signature = value_builder
            .signature()
            .unwrap_or_else(|error| todo!("handle me properly: {error}"));

        // builder.declare_var(Variable, ty);
        // builder.use_var(var);
        for (variable, r#type) in value_builder.local_scope.variables() {
            builder.declare_var(
                variable.into(),
                r#type
                    .map(|r#type| type_store.get(r#type))
                    .unwrap()
                    .clone()
                    .into(),
            );
        }

        for statement in statements {
            match statement {
                local::Statement::Assignment(variable, value) => {
                    let value = value
                        .cranelift_value(&mut builder)
                        .unwrap_or_else(|error| todo!("handle me :( {error}"));

                    builder.def_var(variable.into(), value);
                }
                local::Statement::Ignore(value) => {
                    value
                        .cranelift_value(&mut builder)
                        .unwrap_or_else(|error| todo!("handle me :) {error}"));
                }
                local::Statement::Return(value) => {
                    let value = value
                        .cranelift_value(&mut builder)
                        .unwrap_or_else(|error| todo!("handle me :( {error}"));

                    builder.ins().return_(&[value]);
                }
            };
        }
        
        builder.finalize();

        // Next, declare the function to jit. Functions must be declared
        // before they can be called, or defined.
        //
        // we have a version of `declare_function` that automatically declares
        // the function?
        let id = module
            .declare_function(
                name.as_ref(),
                cranelift_module::Linkage::Export,
                &context.func.signature,
            )
            .unwrap_or_else(|error| todo!("handle me properly: {error:?}"));

        // Define the function to jit. This finishes compilation, although
        // there may be outstanding relocations to perform. Currently, jit
        // cannot finish relocations until all functions to be called are
        // defined. For this toy demo for now, we'll just finalize the
        // function below.
        module
            .define_function(id, &mut context)
            .unwrap_or_else(|error| todo!("handle me properly: {error:?}"));

        // Now that compilation is finished, we can clear out the context state.
        module.clear_context(&mut context);

        // Finalize the functions which we just defined, which resolves any
        // outstanding relocations (patching in addresses, now that they're
        // available).
        module.finalize_definitions().unwrap();

        // We can now retrieve a pointer to the machine code.
        let code = module.get_finalized_function(id);
        let add = unsafe { std::mem::transmute::<*const u8, unsafe fn(i64, i64) -> i64>(code) };
        dbg!(unsafe { add(1, 1) });
    }
}
