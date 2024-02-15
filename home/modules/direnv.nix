{ config, lib, pkgs, ... }:
let inherit (config.modules.direnv) enable extraConfig; in {
  options.modules.direnv.enable = lib.mkEnableOption "Enable direnv module";

  config = lib.mkIf enable {
    pacman.packages = [ "extra/direnv" ];

    modules.bash.extraConfig =
      ''
        eval "$(direnv hook bash)"
      '';

    modules.fish.extraConfig =
      ''
        direnv hook fish | source
      '';

    xdg.configFile = {
      "direnv/direnv.toml".text =
        ''
          [whitelist]
          prefix = ["${config.xdg.userDirs.desktop}"]
        '';
    };
  };
}
