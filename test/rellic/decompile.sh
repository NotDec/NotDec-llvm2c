#!/bin/bash

SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
NOTDEC_BIN=$SCRIPTPATH/../../build/bin/notdec-llvm2c

clang-14 -Xclang -disable-O0-optnone -O0 -c -emit-llvm -S -o ${1}.ll $1
$NOTDEC_BIN ${1}.ll -o ${1}.c
