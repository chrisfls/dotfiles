{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.modules.sublime) enable;
  inherit (specialArgs) mkIfElse;
in
{
  options.modules.sublime.enable = lib.mkEnableOption "Enable sublime module";

  config = lib.mkIf enable {
    home.packages = [
      pkgs.sublime4
      pkgs.sublime-merge
    ];
  };
}
