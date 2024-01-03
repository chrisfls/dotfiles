# TODO: add aur support
{ config, lib, pkgs, ... }:
let
  inherit (config.pacman) enable overrides packages;

  package = prev: path: _:
    let
      pkg = {
        pname = "pacman";
        version = "dummy";
        meta = { };
        passthru = { };
      } // (lib.attrsets.getAttrFromPath path prev);

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
    (lib.attrsets.mapAttrsRecursive (package prev) overrides);

  flatten = attrs:
    lib.lists.concatMap
      (value: if builtins.isAttrs value then flatten value else value)
      (lib.attrsets.attrValues attrs);

  packages-by-name = lib.trivial.pipe overrides [
    (lib.attrsets.mapAttrsRecursive
      (path:
        let inherit (lib.attrsets.getAttrFromPath path pkgs) name;
        in _: [ "${name}" ]))
    flatten
    (map (name: { inherit name; value = true; }))
    builtins.listToAttrs
  ];

  pacman-packages =
    builtins.concatStringsSep " " config.pacman.packages;

  pacman-switch = pkgs.writeShellScriptBin "pacman-switch"
    (builtins.concatStringsSep " && "
      [
        "sudo pacman -Sy --needed --noconfirm archlinux-keyring chaotic-keyring"
        "sudo pacman -Su --needed --noconfirm ${pacman-packages}"
      ]
    );
in
{
  options.pacman = {
    enable = lib.mkEnableOption "Enable pacman module";

    overrides = lib.mkOption {
      type = lib.types.attrs;
      default = { };
    };

    packages = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
    };
  };

  config = lib.mkIf enable {
    home.packages = [ pacman-switch ];
    nixpkgs.overlays = [ (final: overlay) ];
    pacman.packages = (flatten overrides);

    # report packages installed with nix (debug)
    home.file.".nixpkgs".text =
      lib.trivial.pipe config.home.packages [
        (map (pkg: "${pkg.name}"))
        (builtins.filter (name: !(lib.attrsets.hasAttrByPath [ name ] packages-by-name)))
        (builtins.concatStringsSep "\n")
      ];
  };
}
