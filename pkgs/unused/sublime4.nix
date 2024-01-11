{ sublime4, xxd, ... }:
sublime4.overrideAttrs
  (old: {
    sublime_text = old.sublime_text.overrideAttrs
      (_: {
        prePatch =
          # SOURCE: gist [DOT] github [DOT] com [SLASH] skoqaq [SLASH] 3f3e8f28e23c881143cef9cf49d821ff?permalink_comment_id=4786328#gistcomment-4786328
          ''
            sed -i 's;\x80\x78\x05\x00\x0f\x94\xc1;\xc6\x40\x05\x01\x48\x85\xc9;g' "sublime_text"

            if [ $? -ne 0 ]; then
              echo "Failed to apply activation patch" >&2
              exit 1
            fi
          '';
      });
  })
# OTHER METHODS:
#
#   NOT REALLY WORKING WITH LATEST VERSION
#
#   $ echo "00415013: 48 C7 C0 00 00 00 00 C3" | $\{xxd}/bin/xxd -r - sublime_text
#   $ echo "00409037: 90 90 90 90 90" | $\{xxd}/bin/xxd -r - sublime_text
#   $ echo "0040904F: 90 90 90 90 90" | $\{xxd}/bin/xxd -r - sublime_text
#   $ echo "00416CA4: C3" | $\{xxd}/bin/xxd -r - sublime_text
#   $ echo "00414C82: C3" | $\{xxd}/bin/xxd -r - sublime_text
#
#   really brittle patch in this form, made for v4152, but any update will break
#   it, convert it to sed might solve that
#
#   SOURCE: github [DOT] com [SLASH] CodigoCristo [SLASH] sublimepatch [SLASH] blob [SLASH] main [SLASH] src [SLASH] main.c
