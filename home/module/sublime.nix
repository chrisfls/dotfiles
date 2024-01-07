{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.module.sublime) enable;
  inherit (specialArgs) mkIfElse;
  mesa = specialArgs.mesa.wrapIf config.preset.non-nixos;
in
{
  options.module.sublime.enable = lib.mkEnableOption "Enable sublime module";

  config = lib.mkIf enable {
    home.packages = [
      (mesa { pkg = pkgs.sublime4; exe = "sublime_text"; })
      (mesa { pkg = pkgs.sublime-merge; exe = "sublime_merge"; })
    ];

    nixpkgs.config.permittedInsecurePackages = [ "openssl-1.1.1w" ];
  };
}
