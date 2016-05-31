extern crate llvm_sys;
extern crate getopts;
extern crate core;
#[macro_use]
extern crate ende;

use std::env;
use std::fs::File;
use std::io::Read;
use getopts::Options;
use std::ffi::*;
use llvm_sys::core::*;
use core::mem::transmute;
use std::os::raw::c_void;
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
    /*
    let stmt = LetMut("count".to_string(), Literal(10));
    let cond_block = Block { stmts: vec![Mutate("count".to_string(), infix!(Var("count".to_string()), Sub, Literal(1)))],  end: Box::new(Var("count".to_string())) };
    let cond_term = Scope(cond_block);
    let a = vec![Var("count".to_string())];
    let inner_block = Block { stmts: vec![], end: Box::new(Call(FunctionCall { name: "print".to_string() }, a )) };
    let args_types = FunctionTy(vec![I32Ty], Box::new(I32Ty));
    let stmts = vec![Extern("print".to_string(), args_types), stmt];
    let stmt = Scope(Block { stmts: stmts, end: Box::new(While(Box::new(cond_term), inner_block)) });
     */
    let args : Vec<String> = env::args().collect();
    let program = args[0].clone();

    let mut opts = Options::new();
    opts.optopt("o", "", "output file name", "OUTPUT");
    opts.optflag("h", "help", "print this help menu");
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => { m }
        Err(f) => { panic!(f.to_string()) }
    };
    if matches.opt_present("h") {
        print_usage(&program, opts);
        return;
    }
    let output = matches.opt_str("o");
    let input = if !matches.free.is_empty() {
        matches.free[0].clone()
    } else {
        print_usage(&program, opts);
        return;
    };
    let mut input_data = String::new();
    let mut input = File::open(input).unwrap();
    input.read_to_string(&mut input_data).unwrap();
    
    unsafe {
        haskell_init();
        let c_input = CString::new(input_data).unwrap();
        let tree_prim = ende::Parsing::getTree(c_input.into_raw() as *mut c_void);
        let block = to_rust_block(ende::HsClosureFunc::_deRefStablePtr(tree_prim) as *mut ende::HsClosureFunc::StgClosure);
        let result = block.gen_module();
        println!("{:?}", result);
        let module = result.ok().unwrap();
        LLVMDumpModule(module.clone());
        emit_ir(module);
        haskell_exit();
    }
}

fn print_usage(program: &str, opts: Options) {
    let brief = format!("Usage: {} INPUT -o OUTPUT", program);
    print!("{}", opts.usage(&brief));
}
