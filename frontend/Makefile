all: src/Parsing.o


src/Parsing.o: src/Ast.hs src/Parsing.hs last_build
	make clean
	stack build 2> log || (cat log && false)
	./get_args.sh
	stack exec ghc -- src/Ast.hs src/Parsing.hs && ar rcs libParsing.a src/Parsing.o src/Ast.o
	./compile_closure_func.sh
	./bind.sh
	touch last_build
last_build:
	rm -f last_build
clean:
	stack clean
	rm -f ghc_lib_path link_search linker_args all_pkg_path link_lib src/Ast.o src/Ast.hi src/Parsing.o src/Parsing_stub.h Ast.rs libParsing.a last_build
