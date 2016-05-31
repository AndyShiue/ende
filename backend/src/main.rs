extern crate llvm_sys;
extern crate core;
#[macro_use]
extern crate ende;

use llvm_sys::core::*;
use core::mem::transmute;
unsafe fn haskell_init() {
    let filename : &[u8] = b"main\x00";
    let mut argc : i32 = 1;
    let mut argv : & [*const u8] = &[filename.as_ptr(),::core::ptr::null()];
    ende::Rts::hs_init(&mut argc, transmute(&mut argv));
}
unsafe fn haskell_exit() {
    ende::Rts::hs_exit();
}

pub fn main() {
    use ende::ast::*;
    use ende::ast::Operator::*;
    use ende::ast::Term::*;
    use ende::ast::Statement::*;
    use ende::type_check::Type::*;
    use ende::codegen::*;
    use ende::trans::*;

    let stmt = LetMut("count".to_string(), Literal(10));
    let cond_block = Block { stmts: vec![Mutate("count".to_string(), infix!(Var("count".to_string()), Sub, Literal(1)))],  end: Box::new(Var("count".to_string())) };
    let cond_term = Scope(cond_block);
    let a = vec![Var("count".to_string())];
    let inner_block = Block { stmts: vec![], end: Box::new(Call(FunctionCall { name: "print".to_string() }, a )) };
    let args_types = vec![I32Ty];
    let stmts = vec![Extern("print".to_string(), args_types, I32Ty), stmt];
    let stmt = Scope(Block { stmts: stmts, end: Box::new(While(Box::new(cond_term), inner_block)) });

    unsafe {
        haskell_init();
        let tree_prim = ende::Parsing::getTree();
        println!("{:?}", to_rust_block(ende::HsClosureFunc::_deRefStablePtr(tree_prim) as *mut ende::HsClosureFunc::StgClosure));
        println!("{:?}", stmt.clone().gen_module());

        let module = stmt.gen_module().ok().unwrap();
        LLVMDumpModule(module.clone());
        emit_ir(module);
        haskell_exit();
    }
}
