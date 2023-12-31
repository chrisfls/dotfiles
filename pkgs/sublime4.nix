{ sublime4, xxd, ... }:
sublime4.overrideAttrs
  (old: {
    sublime_text = old.sublime_text.overrideAttrs
      (_: {
        prePatch =
          # SAUCE-1: github [DOT] com [SLASH] CodigoCristo [SLASH] sublimepatch [SLASH] blob [SLASH] main [SLASH] src [SLASH] main.c
          # SAUCE-2: gist [DOT] github [DOT] com [SLASH] skoqaq [SLASH] 3f3e8f28e23c881143cef9cf49d821ff?permalink_comment_id=4786328#gistcomment-4786328
          #
          # with `$ xxd -g 1 -c 16 sublime_text`, found at offset:
          # "00489f30: 49 8b 87 98 04 00 00 31 c9 80 78 05 00 0f 94 c1"
          ''
            # TODO: not really working with latest
            # brittle patch, updates will always break it, convert to sed someday
            # echo "00415013: 48 C7 C0 00 00 00 00 C3" | ${xxd}/bin/xxd -r - sublime_text
            # echo "00409037: 90 90 90 90 90" | ${xxd}/bin/xxd -r - sublime_text
            # echo "0040904F: 90 90 90 90 90" | ${xxd}/bin/xxd -r - sublime_text
            # echo "00416CA4: C3" | ${xxd}/bin/xxd -r - sublime_text
            # echo "00414C82: C3" | ${xxd}/bin/xxd -r - sublime_text

            # working patch, thanks to SAUCE-2
            sed -i 's;\x80\x78\x05\x00\x0f\x94\xc1;\xc6\x40\x05\x01\x48\x85\xc9;g' "sublime_text" || (
              echo "Failed to apply activation patch" >&2
              exit 1
            )
          '';
      });
  })

    
    
    
    
    
