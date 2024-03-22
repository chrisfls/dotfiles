{ config, lib, ... }:
let inherit (config.modules.brave) enable; in {
  options.modules.brave.enable = lib.mkEnableOption "Enable brave module";

  config = lib.mkIf enable {
    pacman.packages = [ "chaotic-aur/brave-bin" ];
    #modules.sway.apps."b" = "brave-browser";
  };
}
