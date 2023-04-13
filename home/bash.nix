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
      bashrcExtra = ''
      export PATH="$PATH:$HOME/.nix-profile/bin"
      export DIRENV_LOG_FORMAT=""
      '';
      profileExtra = ''
      # generic shell settings
      [[ -f ~/.bashrc ]] && . ~/.bashrc

      # interactive shell settings
      # export TERM="xterm-256color";
      # export COLORTERM=truecolor
      # export MICRO_TRUECOLOR=1

      unset HISTFILE
      '';
      historyFileSize = 0;
      historySize = 0;
    };
  };
}
