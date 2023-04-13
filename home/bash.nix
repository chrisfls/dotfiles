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
      };
      bashrcExtra = ''
      export PATH="$PATH:$HOME/.nix-profile/bin"
      export DIRENV_LOG_FORMAT=""
      '';
      profileExtra = ''
      unset HISTFILE
      '';

      historyFileSize = 0;
      historySize = 0;
    };
  };
}
