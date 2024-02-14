xxd -c 0 -p "sublime_merge" > "sublime_merge.txt"

orig=$(cat "sublime_merge.txt" | grep -oP '554157415641554154534881ec..240000')

if [ -z "$orig" ]; then
  echo "Failed to find offset" >&2
  exit 1
fi

sed -i "s;$orig;b819010000c3${orig:12};g" "sublime_merge.txt"

if [ $? -ne 0 ]; then
  echo "Failed to apply patch 1" >&2
  exit 1
fi

sed -i 's;ba88130000e8........;ba881300009090909090;g' "sublime_merge.txt"

if [ $? -ne 0 ]; then
  echo "Failed to apply patch 2" >&2
  exit 1
fi

sed -i 's;ba983a0000e8........;ba983a00009090909090;g' "sublime_merge.txt"

if [ $? -ne 0 ]; then
  echo "Failed to apply patch 3" >&2
  exit 1
fi

xxd -c 0 -r -p "sublime_merge.txt" > "sublime_merge"
chmod +x "sublime_merge"
