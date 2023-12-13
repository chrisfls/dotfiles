{ lib, ... }:
let
  toPath = builtins.toPath;
  entryAfter = lib.hm.dag.entryAfter;
in
{ copyFile = orig: dest: entryAfter [ "writeBoundary" ] ''
    $DRY_RUN_CMD cp -u ${toPath orig} ${dest}
    $DRY_RUN_CMD chmod 400 ${dest}
  '';
}
