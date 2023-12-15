{ config, lib, pkgs, ... }:
let
  cfg = config.extra;
  toPath = builtins.toPath;
  entryAfter = lib.hm.dag.entryAfter;
in
{
  options.extra.copyFile = lib.mkOption {
    type = lib.types.attrsOf lib.types.path;
    default = { };
  };

  config = {
    home.activation =
      lib.attrsets.concatMapAttrs
        (dest: orig: {
          "copy-file:${dest}" = entryAfter [ "writeBoundary" ] ''
            $DRY_RUN_CMD mkdir -p $(dirname "${dest}")
            $DRY_RUN_CMD cp -u "${orig}" "${dest}"
            $DRY_RUN_CMD chmod 400 "${dest}"
          '';
        })
        cfg.copyFile;
  };
}
