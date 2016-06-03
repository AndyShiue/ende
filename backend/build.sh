#!/bin/bash
llvm-config --version >/dev/null 2>/dev/null
if [ "$!" == 0 ]; then
	cargo build
else
	PATH=$(dirname $(locate llvm-config |grep bin|head -n 1)):$PATH cargo build
fi
