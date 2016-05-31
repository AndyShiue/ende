extern crate llvm_sys;

pub mod ast;
pub mod type_check;
pub mod codegen;
// pub mod trans;
#[allow(dead_code, non_camel_case_types, non_snake_case)]
pub mod Parsing {
    include!("../../frontend/parsing.rs");
}
#[allow(dead_code, non_camel_case_types, non_snake_case)]
pub mod Rts {
    include!("../../frontend/rts.rs");
}
#[allow(dead_code, non_camel_case_types, non_snake_case)]
pub mod HsClosureFunc {
    include!("../../frontend/hs_closure_func.rs");
}
