{ lib, ... }:
{
  replaceVars = attrs: str:
    builtins.replaceStrings
      (map (key: "\$${key}") (builtins.attrNames attrs))
      (builtins.attrValues attrs)
      str;

  u = str: builtins.fromJSON "\"${str}\"";

  copyFile = orig: dest: lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    $DRY_RUN_CMD cp -u ${builtins.toPath orig} ${dest}
    $DRY_RUN_CMD chmod 400 ${dest}
  '';
}
