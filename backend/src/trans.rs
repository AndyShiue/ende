use HsClosureFunc::*;
use ast::*;
use std::ffi::CStr;
use std::os::raw::c_char;
use std::str::from_utf8;
pub fn to_rust_block<'a>(t : HsStablePtr) -> Block<'a> {
    unsafe {
        let con_desc : *const i8 = _GET_CON_DESC(_get_con_itbl(_UNTAG_CLOSURE(_deRefStablePtr(t) as *mut StgClosure)));
        println!("{}", to_rust_str(con_desc));
    }
    Block {
        stmts : &[],
        end : Box::new(Term::Literal(1))
    }
}
fn to_rust_str(t : *const i8) -> String {
    unsafe {
        from_utf8(CStr::from_ptr(t).to_bytes()).unwrap().to_string()
    }
}
