{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.pacman) enable usr nix packages;

  inherit (builtins) concatStringsSep filter isAttrs isList listToAttrs;
  inherit (lib) mkEnableOption mkIf mkMerge mkOption mkOptionType types getExe;
  inherit (lib.attrsets) attrValues getAttrFromPath hasAttrByPath mapAttrsRecursive recursiveUpdate;
  inherit (lib.lists) concatMap foldl';
  inherit (lib.strings) hasPrefix removePrefix;
  inherit (lib.trivial) pipe;
  inherit (specialArgs) mkIfElse;

  mapPathsRecursive = fn: mapAttrsRecursive (path: _: fn path);

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
      inherit (pkg) pname version;
      src = pkg;
      # TODO: cleanup
      meta = { mainProgram = builtins.baseNameOf (getExe pkg); };
      dontBuild = true;
      dontFixup = true;
      nativeBuildInputs = [ replacer ];
      buildInputs = [ ];
      configureFlags = [ ];
      installPhase = ''
        find . -type f -exec replace "$src" '{}' "$out" ';'
      '';
    };

  overlay = prev: recursiveUpdate prev
    (mapPathsRecursive (package prev) nix);

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

    usr = mkOption {
      type = mkOptionType {
        name = "attrs";
        description = "attribute set";
        check = isAttrs;
        merge = loc: foldl' (res: def: recursiveUpdate res def.value) { };
        emptyValue = { value = { }; };
      };
      default = { };
    };

    nix = mkOption {
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

  config = mkMerge [
    {
      nixpkgs.overlays = mkIfElse enable
        [ (final: prev: (overlay prev) // { usr = mapPathsRecursive (package prev) usr; }) ]
        [ (final: prev: prev // { usr = prev; }) ];
    }
    (mkIf enable {
      home.packages = [ pacman-switch-pkg ];
      pacman.packages = attrLeafs nix ++ attrLeafs usr;
    })
  ];
}
