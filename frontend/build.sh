#!/bin/bash -e
build="0"
if [ ! -e "last_build" ]; then
    build="1"
fi


if [ $build == "1" ]; then
    rm -f last_build
    stack clean
    stack build 2> log || (cat log && false)
    ./get_args.sh
    stack exec ghc -- src/Ast.hs src/Parsing.hs && ar rcs libParsing.a src/Parsing.o src/Ast.o
    ./compile_closure_func.sh
    ./bind.sh
    touch last_build
else
    make lastcomplete
fi
