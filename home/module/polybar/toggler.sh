#!/bin/bash

fst=$1
shift

for arg in "$@"; do
    polybar-msg action "#$arg.module_toggle"
done

polybar-msg action $fst next
