{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.module.bash;
in
{
  options.module.bash = {
    enable = mkEnableOption "bash module";
  };

  config = mkIf cfg.enable {
    home.file = {
      ".bashrc".source = ./.bashrc;
      ".bash_profile".source = ./.bash_profile;
    };
  };
}
