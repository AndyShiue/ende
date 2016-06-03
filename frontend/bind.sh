#!/bin/bash
ghc_libpath=$(cat ghc_lib_path)
export DYLD_LIBRARY_PATH=/Library/Developer/CommandLineTools/usr/lib
../rust-bindgen/target/debug/bindgen -I$ghc_libpath/include/ src/Parsing_stub.h -builtins -o Parsing.rs
../rust-bindgen/target/debug/bindgen -I$ghc_libpath/include/ hs_closure_func.h -builtins -o hs_closure_func.rs
