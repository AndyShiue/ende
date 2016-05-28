#!/bin/bash
ghc_libpath=$(cat ghc_lib_path)
gcc $CFLAGS -I$ghc_libpath/include/ -c hs_closure_func.c -o hs_closure_func.o
ar rcs libHsClosureFunc.a hs_closure_func.o
