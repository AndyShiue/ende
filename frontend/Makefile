all:
	make clean
	stack build 2> log
	./get_args.sh
clean:
	stack clean
	rm ghc_lib_path link_search linker_args all_pkg_path link_lib
