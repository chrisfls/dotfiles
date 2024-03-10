{ config, lib, pkgs, ... }:
let
  inherit (config.modules.emacs) enable;
in
{
  options.modules.emacs.enable = lib.mkEnableOption "Enable emacs module";

  config = lib.mkIf enable {
    pacman.packages = [
      "extra/emacs"
      "aur/semgrep-bin"
      "extra/ripgrep"
    ];

    home.sessionPath = [ "${config.xdg.configHome}/emacs/bin" ];
  };
}
