extern crate llvm_sys;
extern crate core;
#[macro_use]
extern crate ende;

use llvm_sys::core::*;
use core::mem::transmute;
fn haskell_init() {
    let filename : &[u8] = b"main\x00";
    let mut argc : i32 = 1;
    let mut argv : & [*const u8] = &[filename.as_ptr(),::core::ptr::null()];
    unsafe {
        ende::Rts::hs_init(&mut argc, transmute(&mut argv));
    }
}
fn haskell_exit() {
    unsafe {
        ende::Rts::hs_exit();
    }
}

pub fn main() {
    use ende::ast::*;
    use ende::ast::Operator::*;
    use ende::ast::Term::*;
    use ende::ast::Statement::*;
    use ende::compile::*;

    let stmt = LetMut("count", Literal(10));
    let cond_block = Block { stmts: &[Mutate("count", infix!(Var("count"), Sub, Literal(1)))],  end: Box::new(Var("count")) };
    let cond_term = Scope(cond_block);
    let a = &[Var("count")];
    let inner_block = Block { stmts: &[], end: Box::new(Call(FunctionCall { name: "print", arity: 1 }, a )) };
    let stmts = &[stmt];
    let stmt = Scope(Block { stmts: stmts, end: Box::new(While(Box::new(cond_term), inner_block)) });

    unsafe {
        haskell_init();
        let tree_prim = ende::Ast::getTree();
        println!("{:?}", stmt.clone().gen_module());

        let module = stmt.gen_module().ok().unwrap();
        LLVMDumpModule(module.clone());
        emit_ir(module);
        haskell_exit();
    }
}
