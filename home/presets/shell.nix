{ config, lib, pkgs, ... }:
{
  pacman.packages = [
    "core/procps-ng"
    "core/which"
    "extra/jaq"
    "extra/jq"
  ];

  modules = {
    agenix.enable = true;
    bash.enable = true;
    direnv.enable = true;
    keychain.enable = true;
    micro.enable = true;
    fish.enable = true;
    zoxide.enable = true;
    git.enable = true;
  };

  home.sessionVariables = {
    EDITOR = "micro";
    TERM = "xterm-256color";
    COLORTERM = "truecolor";
    MICRO_TRUECOLOR = "1";
    VTE_VERSION = "6003";
    DIRENV_LOG_FORMAT = "";
  };

  targets.genericLinux.enable = true;
}
