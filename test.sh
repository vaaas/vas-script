#!/bin/sh
dir=$(realpath ..)
GUILE_LOAD_PATH=$dir guile compiler.scm -lang $1 $2
