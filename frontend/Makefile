all:
	./build.sh
clean:
	stack clean
	rm -f ghc_lib_path link_search linker_args all_pkg_path link_lib src/Ast.o src/Ast.hi src/Parsing.o src/Parsing_stub.h Ast.rs last_build libParsing.a libAst.a libHsClosureFunc.a hs_closure_func.o hs_closure_func.rs log
lastcomplete: src/Parsing.o


src/Parsing.o: src/Ast.hs src/Parsing.hs
	rm -f last_build
	./build.sh
