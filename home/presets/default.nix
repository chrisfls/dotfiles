{ config, lib, pkgs, ... }:
{
  imports = [
    ./desktop.nix
    ./development.nix
    ./gamedev.nix
    ./stream.nix
    ./work.nix
  ];

  config = {
    pacman.packages = [
      "core/procps-ng"
      "core/which"
      "extra/earlyoom"
      "extra/jaq"
      "extra/jq"
    ];

    modules = {
      agenix.enable = true;
      bash.enable = true;
      direnv.enable = true;
      fish.enable = true;
      git.enable = true;
      keychain.enable = true;
      micro.enable = true;
      zoxide.enable = true;
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
  };
}
