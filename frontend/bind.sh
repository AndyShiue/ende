#!/bin/bash
ghc_libpath=$(cat ghc_lib_path)
export DYLD_LIBRARY_PATH=/Library/Developer/CommandLineTools/usr/lib
../rust-bindgen/target/debug/bindgen -I$ghc_libpath/include/ $ghc_libpath/include/Rts.h -builtins -o rts.rs
../rust-bindgen/target/debug/bindgen -I$ghc_libpath/include/ src/Ast_stub.h -builtins -o Ast.rs
../rust-bindgen/target/debug/bindgen -I$ghc_libpath/include/ hs_closure_func.h -builtins -o hs_closure_func.rs
