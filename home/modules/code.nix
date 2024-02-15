{ config, lib, pkgs, specialArgs, ... }:
let inherit (config.modules.code) enable; in {
  options.modules.code.enable = lib.mkEnableOption "Enable code module";

  config = lib.mkIf enable {
    pacman.packages = [ "chaotic-aur/visual-studio-code-bin" ];
  };
}
