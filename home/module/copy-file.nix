{ config, lib, pkgs, ... }:
let
  inherit (config.module) copyFile;
  inherit (lib.hm.dag) entryAfter;
in
{
  options.module.copyFile = lib.mkOption {
    type = lib.types.attrsOf lib.types.path;
    default = { };
  };

  config.home.activation =
    lib.attrsets.concatMapAttrs
      (dest: orig: {
        "copy-file:${dest}" = entryAfter [ "writeBoundary" ] ''
          $DRY_RUN_CMD mkdir -p $(dirname "${dest}")
          $DRY_RUN_CMD cp -u "${orig}" "${dest}"
        '';
      })
      copyFile;
}
