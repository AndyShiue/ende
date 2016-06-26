#!/bin/bash
LLVM_BIN_PATH=$(cat ../LLVM_BIN_PATH)
PATH=$LLVM_BIN_PATH:$PATH cargo build
