{ config, lib, pkgs, ... }:
let inherit (config.modules.code) enable; in {
  options.modules.code.enable = lib.mkEnableOption "Enable code module";

  config = lib.mkIf enable {
    pacman.packages = [ "chaotic-aur/visual-studio-code-bin" ];

    home.packages = [
      (pkgs.writeHostScriptBin "code"
        ''
          exec /usr/bin/code --enable-features=UseOzonePlatform --ozone-platform=wayland --enable-features=WaylandWindowDecorations "$@"
        '')
    ];
  };
}
