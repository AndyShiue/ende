extern crate llvm_sys;

pub mod ast;
pub mod compile;
#[allow(dead_code, non_camel_case_types, non_snake_case)]
pub mod Ast {
    include!("../../frontend/Ast.rs");
}
#[allow(dead_code, non_camel_case_types, non_snake_case)]
pub mod Rts {
    include!("../../frontend/rts.rs");
}
