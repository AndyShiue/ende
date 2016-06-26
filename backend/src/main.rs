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
    ende::HsClosureFunc::hs_init(&mut argc, transmute(&mut argv));
}
unsafe fn haskell_exit() {
    ende::HsClosureFunc::hs_exit();
}

pub fn main() {
    use ende::codegen::*;
    use ende::trans::*;
    use ende::ast::Position;
    use ende::type_check::{TypeCheck, TaggedProgram};

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
    let output = match matches.opt_str("o") {
        Some(output) => output,
        None => panic!("No output specified")
    };
    let input = if !matches.free.is_empty() {
        matches.free[0].clone()
    } else {
        print_usage(&program, opts);
        return;
    };
    let mut input_data = String::new();
    let mut input = match File::open(input) {
        Ok(result) => result,
        Err(err) => panic!("Failed to open input file: {}", err)
    };
    let _ = input.read_to_string(&mut input_data);

    unsafe {
        haskell_init();
        let c_input = match CString::new(input_data) {
            Ok(c_input) => c_input.into_raw(),
            Err(err) => panic!("Failed to transform input data to c ptr: {}", err)
        };
        let tree_prim = ende::Parsing::parseProgram(c_input as *mut c_void);
        let block : TaggedProgram<Position> = FromHaskellRepr::from_haskell_repr(ende::HsClosureFunc::_deRefStablePtr(tree_prim) as *mut ende::HsClosureFunc::StgClosure);
        let mut env = Map::new();
        let result = block.type_check(&mut env).unwrap().gen_module();
        println!("{:?}", result);
        let module = result.ok().unwrap();
        LLVMDumpModule(module.clone());
        emit_ir(module, output.clone());
        emit_exe(output);
        haskell_exit();
    }
}

fn print_usage(program: &str, opts: Options) {
    let brief = format!("Usage: {} INPUT -o OUTPUT", program);
    print!("{}", opts.usage(&brief));
}
