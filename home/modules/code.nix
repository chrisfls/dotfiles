{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.modules.code) enable;

  pkg =
    pkgs.writeHostScriptBin "code"
      ''
        exec /usr/bin/code --ozone-platform=wayland "$@"
      '';  
in {
  options.modules.code.enable = lib.mkEnableOption "Enable code module";

  config = lib.mkIf enable {
    pacman.packages = [ "chaotic-aur/visual-studio-code-bin" ];

    home.packages = [ pkg ];
  };
}
