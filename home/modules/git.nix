{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.modules.git) enable editor extraConfig;
  ssot = specialArgs.ssot;
in
{
  options.modules.git = {
    enable = lib.mkEnableOption "Enable git module";
    extraConfig = lib.mkOption { type = lib.types.lines; default = ""; };
    editor = lib.mkOption { type = lib.types.str; default = "micro"; };
  };

  config = lib.mkIf enable {
    pacman.packages = [ "extra/git" ];

    xdg.configFile = {
      "git/config".text =
        ''
          [core]
          	editor = "${editor}"
          	excludesfile = "$NIXOS_CONFIG_DIR/scripts/gitignore"

          [init]
          	defaultBranch = "main"

          [pull]
          	rebase = true

          [rerere]
          	enabled = true

          [user]
          	email = "${ssot.contact.email}"
          	name = "${ssot.contact.name}"

          [includeIf "gitdir:${config.home.homeDirectory}/repos/github"]
          	path = "${config.xdg.configHome}/git/config_personal"

          [includeIf "gitdir:${config.home.homeDirectory}/repos/gitlab/"]
          	path = "${config.xdg.configHome}/git/config_gitlab"
        
          ${extraConfig}
        '';
      "git/config_personal".text =
        ''
          [user]
          	email = "${ssot.contact.github.email}"
        '';
      "git/config_gitlab".text =
        ''
          [user]
          	email = "${ssot.contact.gitlab.email}"
        '';
    };
  };
}
