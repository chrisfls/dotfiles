{ sublime4-dev, xxd, ... }:
sublime4-dev.overrideAttrs
  (old: {
    sublime_text = old.sublime_text.overrideAttrs
      (_: {
        prePatch =
          # SAUCE-1: github [DOT] com [SLASH] CodigoCristo [SLASH] sublimepatch [SLASH] blob [SLASH] main [SLASH] src [SLASH] main.c
          # SAUCE-2: gist [DOT] github [DOT] com [SLASH] skoqaq [SLASH] 3f3e8f28e23c881143cef9cf49d821ff?permalink_comment_id=4786328#gistcomment-4786328
          #
          # with `$ xxd -g 1 -c 16 sublime_text`, found at offsets:
          # "0048a5e0: 35 07 00 49 89 c6 49 8b 87 98 04 00 00 31 c9 80"
          # "0048a5f0: 78 05 00 0f 94 c1 8d 14 09 80 78 04 00 8d 5c 09"
          ''
            # TODO: not really working with latest
            # brittle patch, updates will always break it, convert to sed someday
            # echo "00444894: 48 C7 C0 00 00 00 00 C3" | ${xxd}/bin/xxd -r - sublime_text 
            # echo "0042BA70: 90 90 90 90 90" | ${xxd}/bin/xxd -r - sublime_text
            # echo "0042BA88: 90 90 90 90 90" | ${xxd}/bin/xxd -r - sublime_text
            # echo "004467B6: C3" | ${xxd}/bin/xxd -r - sublime_text
            # echo "004444F8: C3" | ${xxd}/bin/xxd -r - sublime_text

            # also not really working, but probably triggers activation after skipping the first dialog
            sed -i 's;\x80\x78\x05\x00\x0f\x94\xc1;\xc6\x40\x05\x01\x48\x85\xc9;g' "sublime_text" || (
              echo "Failed to apply activation patch" >&2
              exit 1
            )
          '';
      });
  })
