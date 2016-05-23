extern crate core;
use core::mem::transmute;
use std::fmt::{self, Display};
#[link(name = "HSrts")]
mod Ast {
    include!("../../frontend/Ast.rs");
}
mod Rts {
    include!("../../frontend/rts.rs");
}
fn haskell_init() {
    let filename : &[u8] = b"main\x00";
    let mut argc : i32 = 1;
    let mut argv : & [*const u8] = &[filename.as_ptr(),::core::ptr::null()];
    unsafe {
        Rts::hs_init(&mut argc, transmute(&mut argv));
    }
}
fn haskell_exit() {
    unsafe {
        Rts::hs_exit();
    }
}
#[test]
fn haskell_bridge() {
    haskell_init();
    unsafe {
        let t = Ast::getTree();
    }
    haskell_exit();
}
