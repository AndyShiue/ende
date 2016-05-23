extern crate llvm_sys;

pub mod ast;
pub mod compile;
pub mod Ast {
    include!("../../frontend/Ast.rs");
}
pub mod Rts {
    include!("../../frontend/rts.rs");
}
