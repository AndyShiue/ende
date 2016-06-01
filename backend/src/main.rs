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
    use ende::codegen::*;
    use ende::trans::*;
    unsafe {
        haskell_init();
        let tree_prim = ende::Parsing::getTree();
        let program = to_rust_program(ende::HsClosureFunc::_deRefStablePtr(tree_prim) as *mut ende::HsClosureFunc::StgClosure);
        let result = program.gen_module();
        println!("{:?}", result);
        let module = result.ok().unwrap();
        LLVMDumpModule(module.clone());
        emit_ir(module);
        haskell_exit();
    }
}
