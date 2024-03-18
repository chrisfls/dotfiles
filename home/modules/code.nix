{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.modules.code) enable;

  # TODO: handle scaling
  pkg =
    pkgs.writeHostScriptBin "code"
      ''
        exec /usr/bin/code --force-device-scale-factor=1.5 "$@"
      '';  
in {
  options.modules.code.enable = lib.mkEnableOption "Enable code module";

  config = lib.mkIf enable {
    pacman.packages = [ "chaotic-aur/visual-studio-code-bin" ];

    home.packages = [ pkg ];
  };
}
