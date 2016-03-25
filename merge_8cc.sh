#!/bin/sh
#
# This script is based on https://github.com/shinh/bflisp/blob/master/merge_8cc.sh

set -e

cat out/libf.h

srcs="error.h list.h dict.h 8cc.h cpp.c debug.c dict.c  error.c gen.c lex.c list.c main.c parse.c string.c"

for c in $srcs; do
    ruby -p0e '$_.sub!(%q(#include "keyword.h"), File.read("8cc/keyword.h"))' \
        < 8cc/$c | grep -v '#include'
done
