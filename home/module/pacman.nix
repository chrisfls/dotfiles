{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.pacman) enable pkgs;

  flatten = attrs:
    lib.lists.concatMap
      (value: if builtins.isAttrs value then flatten value else value)
      (lib.attrsets.attrValues attrs);
in
{
  options.pacman = {
    enable = lib.mkEnableOption "Enable pacman module";

    pkgs = lib.mkOption {
      type = lib.mkOptionType {
        name = "attrs";
        description = "merged attribute set";
        check = builtins.isAttrs;
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

  imports = [
    ./pacman/drivers-switch.nix
    ./pacman/overlay.nix
    ./pacman/pacman-switch.nix
  ];

  config.pacman.packages = flatten pkgs;
}
