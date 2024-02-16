{ config, lib, pkgs, ... }:
let inherit (config.modules.zoxide) enable extraConfig; in {
  options.modules.zoxide.enable = lib.mkEnableOption "Enable zoxide module";

  config = lib.mkIf enable {
    pacman.packages = [ "extra/zoxide" ];

    modules.bash.extraConfig =
      ''
        eval "$(zoxide init bash)"
      '';

    modules.fish.extraConfig =
      ''
        zoxide init fish | source
      '';
  };
}
