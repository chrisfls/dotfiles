{ config, lib, pkgs, ... }:
let inherit (config.modules.asdf) enable extraConfig; in {
  options.modules.asdf.enable = lib.mkEnableOption "Enable asdf module";

  config = lib.mkIf enable {
    pacman.packages = [ 
      "chaotic-aur/asdf-vm"
    ];

    modules.bash.extraConfig =
      ''
        . /opt/asdf-vm/asdf.sh
      '';

    modules.fish.extraConfig =
      ''
        source /opt/asdf-vm/asdf.fish
      '';
  };
}
