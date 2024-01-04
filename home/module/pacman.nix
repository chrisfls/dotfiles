# TODO: add aur support
{ config, lib, pkgs, ... }:
let
  inherit (builtins) concatStringsSep filter isAttrs isList listToAttrs;
  inherit (config.pacman) enable overrides packages;
  inherit (lib.attrsets) attrValues getAttrFromPath hasAttrByPath mapAttrsRecursive recursiveUpdate;
  inherit (lib.lists) concatMap foldl';
  inherit (lib.trivial) pipe;
  inherit (lib.strings) hasPrefix removePrefix;
  inherit (lib) mkEnableOption mkIf mkOption mkOptionType types;

  usr =
    pkgs.stdenvNoCC.mkDerivation {
      pname = "usr";
      version = "latest";
      dontUnpack = true;
      dontBuild = true;
      dontFixup = true;
      installPhase = ''
        ln -s "/usr/" "$out"
      '';
    };

  # base package to copy files from pacman into nix
  package = prev: path:
    let
      pkg = {
        pname = "pacman";
        version = "dummy";
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
      inherit (pkg) pname version meta passthru;
      src = pkg;
      dontBuild = true;
      dontFixup = true;
      nativeBuildInputs = [ replacer ];
      buildInputs = [];
      configureFlags = [];
      installPhase = ''
        find . -type f -exec replace "$src" '{}' "$out" ';'
      '';
    };

  # mapAttrsRecursive without value
  mapPathsRecursive = fn: mapAttrsRecursive (path: _: fn path);

  # merge replace overrides with packages and merges it into prev
  overlay = prev: recursiveUpdate prev
    (mapPathsRecursive (package prev) overrides);

  repo-packages =
    filter (name: !(hasPrefix "aur/" name)) packages;

  aur-packages = filter (name: hasPrefix "aur/" name) packages;

  # install or update all packages required by nix  
  pacman-switch-pkg = pkgs.writeShellScriptBin "pacman-switch"
    (concatStringsSep " && "
      [
        "sudo pacman -Sy --needed --noconfirm archlinux-keyring chaotic-keyring"
        "sudo pacman -Su --needed --noconfirm ${concatStringsSep " " repo-packages}"
        "paru -Sua --needed --noconfirm ${concatStringsSep " " aur-packages}"
      ]
    );

  # fetches all the leafs of an attrset
  attrLeafs = attrs:
    concatMap
      (value:
        if isAttrs value then
          attrLeafs value
        else if isList value then
          value
        else
          [ value ])
      (attrValues attrs);

  concatLines =
    concatStringsSep "\n";
in
{
  options.pacman = {
    enable = mkEnableOption "Enable pacman module";

    overrides = mkOption {
      type = mkOptionType {
        name = "attrs";
        description = "attribute set";
        check = isAttrs;
        merge = loc: foldl' (res: def: recursiveUpdate res def.value) { };
        emptyValue = { value = { }; };
      };
      default = { };
    };

    packages = mkOption {
      type = types.listOf types.str;
      default = [ ];
    };
  };

  config = mkIf enable {
    home.packages = [ pacman-switch-pkg ];
    nixpkgs.overlays = [ (final: overlay) ];
    pacman.packages = attrLeafs overrides;

    # report packages installed with nix (debug)
    home.file.".nixpkgs".text =
      let
        packages-by-name = pipe overrides [
          (mapPathsRecursive (path: (getAttrFromPath path pkgs).name))
          attrLeafs
          (map (name: { inherit name; value = true; }))
          listToAttrs
        ];
      in
      pipe config.home.packages [
        (map (pkg: "${pkg.name}"))
        (filter (name: !(hasAttrByPath [ name ] packages-by-name)))
        concatLines
      ];

    home.file.".pacman".text =
      pipe overrides [
        (mapPathsRecursive (path: (concatStringsSep "." path)))
        attrLeafs
        concatLines
      ];
  };
}
