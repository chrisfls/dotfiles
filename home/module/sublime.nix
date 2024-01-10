{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.module.sublime) enable;
  inherit (specialArgs) mkIfElse;
in
{
  options.module.sublime.enable = lib.mkEnableOption "Enable sublime module";

  config = lib.mkIf enable {
    home.packages = [
      pkgs.sublime4
      pkgs.sublime-merge
    ];
  };
}
