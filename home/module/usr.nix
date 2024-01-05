{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config) usr;

  inherit (builtins) isAttrs;
  inherit (lib) getExe mkOption mkOptionType;
  inherit (lib.attrsets) getAttrFromPath mapAttrsRecursive recursiveUpdate;
  inherit (lib.lists) foldl';
  inherit (specialArgs) mkIfElse;

  usr-pkg = prev: path:
    let
      pkg = {
        pname = "usr";
        version = "latest";
        meta = { };
        passthru = { };
      } // (getAttrFromPath path prev);

      replacer = prev.writeShellScriptBin "replace" ''
        set -eu
        file="''${2#./}"
        echo "-> /usr/$file:$3/$file"
        mkdir -p $(dirname "$3/$file")
        ln -s "/usr/$file" "$3/$file"
      '';
    in
    prev.stdenvNoCC.mkDerivation {
      inherit (pkg) pname version;
      src = pkg;
      meta = pkg.meta // { mainProgram = builtins.baseNameOf (getExe pkg); };
      dontBuild = true;
      dontFixup = true;
      nativeBuildInputs = [ replacer ];
      buildInputs = [ ];
      configureFlags = [ ];
      installPhase = ''
        find . -type f -exec replace "$src" '{}' "$out" ';'
      '';
    };

  nix-pkg = prev: path: getAttrFromPath path prev;

  mapPathsRecursive = fn: mapAttrsRecursive (path: _: fn path);
in
{
  options.usr = mkOption {
    type = mkOptionType {
      name = "attrs";
      description = "attribute set";
      check = isAttrs;
      merge = loc: foldl' (res: def: recursiveUpdate res def.value) { };
      emptyValue = { value = { }; };
    };
    default = { };
  };

  config.nixpkgs.overlays =
    mkIfElse config.targets.genericLinux.enable
      [ (final: prev: prev // { usr = mapPathsRecursive (usr-pkg prev) usr; }) ]
      [ (final: prev: prev // { usr = mapPathsRecursive (nix-pkg prev) usr; }) ];
}
