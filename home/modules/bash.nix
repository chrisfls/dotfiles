{ config, lib, pkgs, ... }:
let inherit (config.modules.bash) enable extraConfig; in {
  options.modules.bash.enable = lib.mkEnableOption "Enable bash module";
  options.modules.bash.extraConfig = lib.mkOption { type = lib.types.lines; default = ""; };

  config = lib.mkIf enable {
    pacman.packages = [ "extra/bash" ];

    home.file = {
      ".bashrc".text =
        ''
          # Commands that should be applied only for interactive shells.
          [[ $- == *i* ]] || return

          HISTFILESIZE=0
          HISTSIZE=0

          shopt -s histappend
          shopt -s checkwinsize
          shopt -s extglob
          shopt -s globstar
          shopt -s checkjobs

          . "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"

          ${extraConfig}
        '';
      ".bash_profile".text =
        ''
          # include .profile if it exists
          [[ -f ~/.profile ]] && . ~/.profile

          # include .bashrc if it exists
          [[ -f ~/.bashrc ]] && . ~/.bashrc
        '';
      ".profile".text =
        ''
          . "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
          unset HISTFILE
        '';
    };
  };
}
