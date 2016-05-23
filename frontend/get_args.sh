#!/bin/bash
x=$(cat log | grep -E2 Linker |tail -n 2|head -n 1 |sed 's/^.*\(-L.*\)/\1/g'|sed 's/-W/-/g')
args=""
ghc_lib_path=""
link_lib=""
link_search=""
include=""
for word in $x ; do
    if [ "-" = "$(echo $word | head -c 1)" ]; then
	echo $word | grep '-lHSende\|search_paths_first\|,-u,' > /dev/null
	if [ $? -ne 0 ]; then
	    args="$word $args"
	fi
    fi
    if [ "-L" = "$(echo $word | head -c 2)" ]; then
	#lib path
	link_search="$(echo $word | tail -c +3) $link_search"
	echo $word | grep ".*ghc.*rts" > /dev/null
	if [ $? -eq 0 ]; then
	    temp=$(echo "$word/.." | tail -c +3)
	    if [ -e "$temp/include/HsFFI.h" ]; then
		temp=$(realpath $temp)
		echo "Found ghc lib path: $temp"
		ghc_lib_path=$temp
	    fi
	fi
    fi
    if [ "-l" = "$(echo $word | head -c 2)" ]; then
	#lib
	echo $word | grep '-lHSende\|search_paths_first\|,-u,' > /dev/null
	if [ $? -ne 0 ]; then
	    link_lib="$(echo $word | tail -c +3) $link_lib"
	fi
    fi
done
echo $ghc_lib_path > ghc_lib_path
echo $link_lib > link_lib
echo $link_search > link_search
echo $args > linker_args
pkg_path_pre=$(echo $ghc_lib_path/*)
pkg_path=""
for word in $pkg_path_pre ; do
    if [ -d "$word" ] ; then
	pkg_path="$word $pkg_path"
    fi
done
echo $pkg_path > all_pkg_path
