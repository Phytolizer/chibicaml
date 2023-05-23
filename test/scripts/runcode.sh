#!/bin/sh
set -euo pipefail
chibic "$1" > tmp.asm
nasm -felf64 tmp.asm
gcc -static -z noexecstack -o tmp tmp.o tmp2.o
./tmp
