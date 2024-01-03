{ config, lib, pkgs, ... }:
let
  inherit (config.module.pacman) enable packages;

  package = prev: path: _:
    let
      pkg = lib.attrsets.getAttrFromPath path prev;

      replacer = prev.writeShellScriptBin "replace" ''
        set -eu
        file="''${2#./}"
        echo "-> /usr/$file:$3/$file"
        mkdir -p $(dirname "$3/$file")
        ln -s "/usr/$file" "$3/$file"
      '';
    in
    prev.stdenvNoCC.mkDerivation {
      inherit (pkg) pname version meta passthru;
      src = pkg;
      dontBuild = true;
      dontFixup = true;
      nativeBuildInputs = [ replacer ];
      installPhase = ''
        find . -type f -exec replace "$src" '{}' "$out" ';'
      '';
    };

  overlay = prev: lib.attrsets.recursiveUpdate prev
    (lib.attrsets.mapAttrsRecursive (package prev) packages);

  flatten = attrs:
    lib.lists.concatMap
      (value: if builtins.isAttrs value then flatten value else value)
      (lib.attrsets.attrValues attrs);

  pacman-packages =
    builtins.concatStringsSep " " (flatten packages);

  pacman-switch = pkgs.writeShellScriptBin "pacman-switch"
    (builtins.concatStringsSep " && "
      [
        "sudo pacman -Sy --needed --noconfirm archlinux-keyring chaotic-keyring"
        "sudo pacman -Su --needed --noconfirm ${pacman-packages}"
      ]
    );
in
{
  options.module.pacman = {
    enable = lib.mkEnableOption "Enable pacman module";

    packages = lib.mkOption {
      type = lib.types.attrs;
      default = { };
    };
  };

  config = lib.mkIf enable {
    home.packages = [ pacman-switch ];
    nixpkgs.overlays = [ (final: overlay) ];
  };
}
