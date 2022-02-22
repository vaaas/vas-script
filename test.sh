#!/bin/sh
dir=$(realpath ..)
GUILE_LOAD_PATH=$dir guile vsc.scm -lang $1 $2
