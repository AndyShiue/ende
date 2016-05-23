use std::fs::File;
use std::io::Read;
use std::env;
fn main() {
    let mut search_data = String::new();
    let mut lib_data = String::new();
    let mut pkg_data = String::new();
    let mut ghc_lib_path_data = String::new();
    let mut link_search = File::open("../frontend/link_search").unwrap();
    let mut ghc_lib_path = File::open("../frontend/ghc_lib_path").unwrap();
    let mut link_lib = File::open("../frontend/link_lib").unwrap();
    let mut pkg = File::open("../frontend/all_pkg_path").unwrap();
    link_search.read_to_string(&mut search_data).unwrap();
    link_lib.read_to_string(&mut lib_data).unwrap();
    pkg.read_to_string(&mut pkg_data).unwrap();
/*
    for i in search_data.split(' ') {
        if i != "" {
            println!("cargo:rustc-libdir={}", i);
        }
    }
*/

    let mut linker_args_data = String::new();
    let mut linker_args = File::open("../frontend/linker_args").unwrap();
    linker_args.read_to_string(&mut linker_args_data).unwrap();

    for i in linker_args_data.split(' ') {
        if i != "" {
            let bytes = i.as_bytes();
            if bytes.len() > 2 {
                use std::str::from_utf8;
                println!("cargo:rustc-flags={} {}", from_utf8(&bytes[0..2]).unwrap(), from_utf8(&bytes[2..(bytes.len())]).unwrap());
            }
        }
    }
    for i in pkg_data.split(' ') {
        if i != "" {
            if i.contains("base") || i.contains("gmp") || i.contains("integ") || i.contains("ghcpr") {
                println!("cargo:rustc-flags={} {}", "-L", i);
            }
        }
    }

    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    println!("cargo:rustc-flags=-L {}/../frontend", manifest_dir);
    println!("cargo:rustc-link-lib=static=Ast");
//    panic!("asdf");
}
