all:
	make clean
	stack build 2> log || (cat log && false)
	./get_args.sh
	stack exec ghc -- src/Ast.hs && ar rcs libAst.a src/Ast.o
	./compile_closure_func.sh
	./bind.sh
clean:
	stack clean
	rm ghc_lib_path link_search linker_args all_pkg_path link_lib src/Ast.o src/Ast_stub.h src/Ast.hi Ast.rs rts.rs libAst.a || true
