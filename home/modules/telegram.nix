{ config, lib, pkgs, specialArgs, ... }:
let inherit (config.modules.telegram) enable; in {
  options.modules.telegram.enable = lib.mkEnableOption "Enable telegram module";

  config = lib.mkIf enable {
    # TODO: delete module
    # pacman.packages = [ "extra/telegram-desktvop" ];
  };
}
