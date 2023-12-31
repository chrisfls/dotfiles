# this package overlay is not ready and probably never will be
{ sublime4-dev, xxd, ... }:
sublime4-dev.overrideAttrs
  (old: {
    sublime_text = old.sublime_text.overrideAttrs
      (_: {
        prePatch =
          # SOURCES
          #   - first patches: 
          #     - gist [DOT] github [DOT] com [SLASH] skoqaq [SLASH] 3f3e8f28e23c881143cef9cf49d821ff?permalink_comment_id=4774625#gistcomment-4774625
          #   - last patch:
          #     - gist [DOT] github [DOT] com [SLASH] skoqaq [SLASH] 3f3e8f28e23c881143cef9cf49d821ff?permalink_comment_id=4786328#gistcomment-4786328
          ''
            # WORKING, BUT BREAKS NIX PACKAGE
            
            ${xxd}/bin/xxd -c 0 -p sublime_text > sublime_text.txt
            
            sed -i 's;554157415641554154534881ec..240000;b800000000c3554154534881ec58240000;g' "sublime_text.txt"

            if [ $? -ne 0 ]; then
              echo "Failed to apply patch 1" >&2
              exit 1
            fi
            
            sed -i 's;ba88130000e8........;ba881300009090909090;g' "sublime_text.txt"

            if [ $? -ne 0 ]; then
              echo "Failed to apply patch 2" >&2
              exit 1
            fi

            sed -i 's;ba983a0000e8........;ba983a00009090909090;g' "sublime_text.txt"

            if [ $? -ne 0 ]; then
              echo "Failed to apply patch 3" >&2
              exit 1
            fi
            
            # extra patch (not really needed)
            sed -i 's;807805000f94c1;c64005014885c9;g' "sublime_text.txt"

            if [ $? -ne 0 ]; then
              echo "Failed to apply activation patch" >&2
              exit 1
            fi
            
            ${xxd}/bin/xxd -c 0 -r -p sublime_text.txt > $out/bin/sublime_text
          ''
          });
      })
# OTHER METHODS:
#
#   NOT REALLY WORKING WITH LATEST (MADE FOR 4156)
#
#   $ echo "00444894: 48 C7 C0 00 00 00 00 C3" | xxd -r - sublime_text 
#   $ echo "0042BA70: 90 90 90 90 90" | xxd -r - sublime_text
#   $ echo "0042BA88: 90 90 90 90 90" | xxd -r - sublime_text
#   $ echo "004467B6: C3" | xxd -r - sublime_text
#   $ echo "004444F8: C3" | xxd -r - sublime_text
#
#   really brittle patch in this form, made for v4156, but any update will break
#   it, convert it to sed might solve that
#
#   SOURCE: github [DOT] com [SLASH] CodigoCristo [SLASH] sublimepatch [SLASH] blob [SLASH] main [SLASH] src [SLASH] main.c
#
#   ---
#
#   ALSO NOT WORKING, ALSO BREAKS NIX PACKAGE
#   
#   $ sed -i 's;\x41\x57\x41\x56\x56\x57\x55\x53\xB8\x28\x21\x00\x00;33\xC0\xFE\xC0\xC3\x57\x55\x53\xB8\x28\x21\x00\x00;g' \
#       "sublime_text"
#   $ sed -i 's;\x6C\x69\x63\x65\x6E\x73\x65\x2E\x73\x75\x62\x6C\x69\x6D\x65\x68\x71\x2E\x63\x6F\x6D;73\x75\x62\x6C\x69\x6D\x65\x68\x71\x2E\x6C\x6F\x63\x61\x6C\x68\x6F\x73\x74\x00\x00;g' \
#       "sublime_text"
#
#   made for an older (unknown) version
#
#   SOURCE: nairatag [DOT] com [SLASH] programming [SLASH] crack-sublime-text-4-easily
