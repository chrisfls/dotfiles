{ config, lib, pkgs, ... }:
let inherit (config.modules.vfox) enable extraConfig; in {
  options.modules.vfox.enable = lib.mkEnableOption "Enable vfox module";

  config = lib.mkIf enable {
    pacman.packages = [ "aur/vfox-bin" ];

    modules.bash.extraConfig =
      ''
        eval "$(vfox activate bash)"
      '';

    modules.fish.extraConfig =
      ''
        vfox activate fish | source
      '';
  };
}
