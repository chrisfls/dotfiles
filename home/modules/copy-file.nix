{ config, lib, pkgs, ... }:
let
  inherit (config.modules) copyFile;
  inherit (lib.hm.dag) entryAfter;
in
{
  options.modules.copyFile = lib.mkOption {
    type = lib.types.attrsOf lib.types.path;
    default = { };
  };

  config.home.activation =
    lib.attrsets.concatMapAttrs
      (dest: orig: {
        "copy-file:${dest}" = entryAfter [ "writeBoundary" ] ''
          $DRY_RUN_CMD mkdir -p $(dirname "${dest}")
          $DRY_RUN_CMD cp -Ru "${orig}" "${dest}"
        '';
      })
      copyFile;
}
