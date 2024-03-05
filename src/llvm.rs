use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::OptimizationLevel;
use crate::semantic::TypedAst;


pub fn codegen(tast: TypedAst) {
    let context = Context::create();
    let module = context.create_module("mod");

    let i8 = context.i8_type();
    let i16 = context.i16_type();
    let i32 = context.i32_type();
    let i64 = context.i64_type();

}