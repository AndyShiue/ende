#!/bin/bash
llvm-config --version >/dev/null 2>/dev/null
if [ "$?" == 0 ]; then
	cargo build
else
	PATH=$(brew --prefix llvm)/bin:$PATH cargo build
fi
