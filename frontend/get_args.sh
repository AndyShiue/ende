#!/bin/bash
x=$(cat log | grep -E2 Linker |tail -n 2|head -n 1 |sed 's/^.*\(-L.*\)/\1/g'|sed 's/-W/-/g' |sed 's/,-u,//g')
args=""
ghc_lib_path=""
for word in $x ; do
    if [ "-" = "$(echo $word | head -c 1)" ]; then
	args="$word $args"
    fi
    if [ "-L" = "$(echo $word | head -c 2)" ]; then
	echo $word | grep ".*ghc.*rts" 
	if [ $? -eq 0 ]; then
	    temp=$(echo "$word/.." | tail -c +3)
	    if [ -e "$temp/include/HsFFI.h" ]; then
		ghc_lib_path=$(realpath $temp)
	    fi
	fi
    fi
done
echo $ghc_lib_path > ghc_lib_path
    
echo $args > linker_args
