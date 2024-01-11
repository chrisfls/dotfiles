{ config, lib, pkgs, ... }:
let
  inherit (config.pacman) enable;

  pkgs' = config.pacman.pkgs;

  nix-pkg = path: prev:
    let
      path' =
        if builtins.head path == "usr" then
          builtins.tail path
        else
          path;
    in
    lib.attrsets.getAttrFromPath path' prev;

  usr-pkg = prev: path:
    let
      pkg = {
        pname = "pacman";
        version = "usr";
        meta = { };
        passthru = { };
      } // (nix-pkg path prev);

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
      meta = pkg.meta;
      dontBuild = true;
      dontFixup = true;
      nativeBuildInputs = [ replacer ];
      buildInputs = [ ];
      configureFlags = [ ];
      installPhase = ''
        find . -type f -exec replace "$src" '{}' "$out" ';'
      '';
    };

  package =
    if config.preset.non-nixos then
      usr-pkg

    else
      nix-pkg;

  mapPathsRecursive = fn: lib.attrsets.mapAttrsRecursive (path: _: fn path);

  overlay = prev: lib.attrsets.recursiveUpdate prev
    (mapPathsRecursive (package prev) pkgs');
in
lib.mkIf enable {
  nixpkgs.overlays = [ (final: overlay) ];
}
