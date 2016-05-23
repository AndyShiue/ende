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
    let mut search_args : Vec<String> = Vec::new();
    let mut lib_args : Vec<String> = Vec::new();
    for i in search_data.split(' ') {
        if i != "" {
            search_args.push("-L".to_string());
            search_args.push(i.to_string());
        }
    }
    for i in lib_data.split(' ') {
        if i != "" {
            lib_args.push("-l".to_string());
            lib_args.push(i.to_string());
        }
    }
    println!("cargo:rustc-flags={}", search_args.join(" "));
    println!("cargo:rustc-flags={}", lib_args.join(" "));

    let mut pkg_args : Vec<String> = Vec::new();
    for i in pkg_data.split(' ') {
        if i != "" {
            if i.contains("base") || i.contains("gmp") || i.contains("integ") || i.contains("ghcpr") {
                pkg_args.push("-L".to_string());
                pkg_args.push(i.to_string());
            }
        }
    }

    println!("cargo:rustc-flags={}", pkg_args.join(" "));

    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    println!("cargo:rustc-flags=-L {}/../frontend", manifest_dir);
    println!("cargo:rustc-link-lib=static=Ast");
//    panic!("sf");
}
