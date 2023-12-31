#!/bin/bash

# tested with sublime_merge_build_2092_x64.tar.xz

xxd -c 0 -p $1 > "$1.txt"

orig=$(cat $1.txt | grep -oP '554157415641554154534881ec..240000')

if [ -z "$orig" ]; then
  echo "Failed to find offset" >&2
  exit 1
fi

sed -i "s;$orig;48c7c001000000c3${orig:16};g" "$1.txt"

if [ $? -ne 0 ]; then
  echo "Failed to apply patch 1" >&2
  exit 1
fi

sed -i 's;ba88130000e8........;ba881300009090909090;g' "$1.txt"

if [ $? -ne 0 ]; then
  echo "Failed to apply patch 2" >&2
  exit 1
fi

sed -i 's;ba983a0000e8........;ba983a00009090909090;g' "$1.txt"

if [ $? -ne 0 ]; then
  echo "Failed to apply patch 3" >&2
  exit 1
fi

sed -i 's;f04883c4085b415ec3..56534881ec1803;f04883c4085b415ec3c356534881ec1803;g' "$1.txt" # '..' should be '41'

if [ $? -ne 0 ]; then
  echo "Failed to apply patch 4" >&2
  exit 1
fi

sed -i 's;c3488d05fbcc5400c3..4156534189f648;c3488d05fbcc5400c3c34156534189f648;g' "$1.txt" # '..' should be '55'

if [ $? -ne 0 ]; then
  echo "Failed to apply patch 5" >&2
  exit 1
fi

xxd -c 0 -r -p $1.txt > "${1}_patched"
chmod +x "${1}_patched"
rm $1.txt
