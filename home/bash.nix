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
    programs.bash = {
      enable = true;
      enableVteIntegration = true;
      sessionVariables = {
        EDITOR = "micro";
        TERM = "xterm-256color";
        COLORTERM = "truecolor";
        MICRO_TRUECOLOR = "1";
        VTE_VERSION = "6003";
        DIRENV_LOG_FORMAT = "";
        PATH = "$PATH:$HOME/.nix-profile/bin";
        DISPLAY = "$(ip route list default | awk '{print $3}'):0";
        LIBGL_ALWAYS_INDIRECT = "1";
      };
      profileExtra = ''
      unset HISTFILE
      '';
      historyFileSize = 0;
      historySize = 0;
    };
  };
}
