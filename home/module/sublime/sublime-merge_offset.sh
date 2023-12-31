#!/bin/bash

# tested with sublime_merge_build_2091_x64.tar.xz
cp -f ./sublime_merge/sublime_merge "${1}_patched"

echo "004E1752: 48 C7 C0 01 00 00 00 C3" | xxd -r - "${1}_patched"
echo "004E48DD: 90 90 90 90 90"          | xxd -r - "${1}_patched"
echo "004E48F5: 90 90 90 90 90"          | xxd -r - "${1}_patched"
echo "004E2FD6: C3"                      | xxd -r - "${1}_patched"
echo "004E145C: C3"                      | xxd -r - "${1}_patched"
