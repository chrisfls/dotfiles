# TODO: add aur support
{ config, lib, pkgs, ... }:
let
  inherit (config.pacman) enable overrides packages;

  # base package to copy files from pacman into nix
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

  # merge replace overrides with packages and merges it into prev
  overlay = prev: lib.attrsets.recursiveUpdate prev
    (lib.attrsets.mapAttrsRecursive (package prev) overrides);

  # install or update all packages required by nix  
  pacman-switch-pkg = pkgs.writeShellScriptBin "pacman-switch"
    (builtins.concatStringsSep " && "
      [
        "sudo pacman -Sy --needed --noconfirm archlinux-keyring chaotic-keyring"
        "sudo pacman -Su --needed --noconfirm ${builtins.concatStringsSep " " packages}"
      ]
    );

  # fetches all the leafs of an attrset
  attrLeafs = attrs:
    lib.lists.concatMap
      (value:
        if builtins.isAttrs value then
          attrLeafs value
        else if builtins.isList value then
          value
        else
          [ value ])
      (lib.attrsets.attrValues attrs);

  concatLines =
    builtins.concatStringsSep "\n";
in
{
  options.pacman = {
    enable = lib.mkEnableOption "Enable pacman module";

    overrides = lib.mkOption {
      type = lib.mkOptionType {
        name = "attrs";
        description = "attribute set";
        check = lib.isAttrs;
        merge = loc: lib.lists.foldl' (res: def: lib.attrsets.recursiveUpdate res def.value) { };
        emptyValue = { value = { }; };
      };
      default = { };
    };

    packages = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
    };
  };

  config = lib.mkIf enable {
    home.packages = [ pacman-switch-pkg ];
    nixpkgs.overlays = [ (final: overlay) ];
    pacman.packages = attrLeafs overrides;

    # report packages installed with nix (debug)
    home.file.".nixpkgs".text =
      let
        packages-by-name = lib.trivial.pipe overrides [
          (lib.attrsets.mapAttrsRecursive (path: _: (lib.attrsets.getAttrFromPath path pkgs).name))
          attrLeafs
          (map (name: { inherit name; value = true; }))
          builtins.listToAttrs
        ];
      in
      lib.trivial.pipe config.home.packages [
        (map (pkg: "${pkg.name}"))
        (builtins.filter (name: !(lib.attrsets.hasAttrByPath [ name ] packages-by-name)))
        concatLines
      ];

    home.file.".pacman".text =
      lib.trivial.pipe overrides [
        (lib.attrsets.mapAttrsRecursive (path: _: [ (builtins.concatStringsSep "." path) ]))
        attrLeafs
        concatLines
      ];
  };
}
