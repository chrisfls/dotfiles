{ config, lib, pkgs, ... }:
let inherit (config.modules.asdf) enable extraConfig; in {
  options.modules.asdf.enable = lib.mkEnableOption "Enable asdf module";

  config = lib.mkIf enable {
    pacman.packages = [ 
      "chaotic-aur/asdf-vm"
      "chaotic-aur/postman-bin"
      "chaotic-aur/heroku-cli-bin"
      "core/ncurses"
      "core/unixodbc"
      "extra/fop"
      "extra/libssh"
      "extra/libxslt"
      "extra/wxwidgets-gtk3"
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
