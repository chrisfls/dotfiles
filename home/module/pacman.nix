{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.pacman) enable packages;
  inherit (builtins)
    concatStringsSep
    filter
    head
    isAttrs
    isList
    tail;
  inherit (lib)
    getExe
    mkEnableOption
    mkIf
    mkMerge
    mkOption
    mkOptionType
    types;
  inherit (lib.attrsets)
    attrValues
    hasAttrByPath
    getAttrFromPath
    mapAttrsRecursive
    recursiveUpdate;
  inherit (lib.lists) concatMap foldl';
  inherit (lib.strings) hasPrefix;
  inherit (lib.trivial) pipe;
  inherit (specialArgs) mkIfElse;

  pkgs' = config.pacman.pkgs;

  mergedAttrs = mkOptionType {
    name = "attrs";
    description = "merged attribute set";
    check = isAttrs;
    merge = loc: foldl' (res: def: recursiveUpdate res def.value) { };
    emptyValue = { value = { }; };
  };

  mapPathsRecursive = fn: mapAttrsRecursive (path: _: fn path);
in
{
  options.pacman = {
    enable = mkEnableOption "Enable pacman module";

    pkgs = mkOption {
      type = mergedAttrs;
      default = { };
    };

    packages = mkOption {
      type = types.listOf types.str;
      default = [ ];
    };
  };

  config.pacman.packages =
    let
      flatten = attrs:
        concatMap
          (value: if isAttrs value then flatten value else value)
          (attrValues attrs);
    in
    flatten pkgs';

  config.nixpkgs.overlays =
    let
      nix-pkg = path: prev:
        let
          path' =
            if head path == "usr" then
              tail path
            else
              path;
        in
        getAttrFromPath path' prev;

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

      package =
        if config.preset.non-nixos then
          usr-pkg

        else
          nix-pkg;

      overlay = prev: recursiveUpdate prev
        (mapPathsRecursive (package prev) pkgs');
    in
    [ (final: overlay) ];

  config.home.packages =
    let
      quote = str: "'${str}'";

      repo-packages = pipe packages [
        (filter (name: !(hasPrefix "aur/" name)))
        (map quote)
      ];

      aur-packages = pipe packages [
        (filter (name: hasPrefix "aur/" name))
        (map quote)
      ];
    in
    mkIf enable [
      (pkgs.writeShellScriptBin "pacman-switch"
        ''
          sudo pacman -Sy --needed --noconfirm archlinux-keyring chaotic-keyring && \
          sudo pacman -Su --needed --noconfirm ${concatStringsSep " " repo-packages} && \
          paru -Sua --needed --noconfirm ${concatStringsSep " " aur-packages}
        '')
    ];
}
