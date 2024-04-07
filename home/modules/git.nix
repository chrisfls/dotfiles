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
    pacman.packages = [
      "extra/git"
      "extra/git-lfs"
    ];

    home.file.".ssh/config".text =
      ''
        Host *
          PreferredAuthentications publickey
          IdentityFile ~/.ssh/id_ed25519
      '';

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
          	name = "${ssot.contact.name}"
          	email = "${ssot.contact.github.email}"

          [includeIf "gitdir:${config.home.homeDirectory}/Desktop/personal/gitlab/"]
          	path = "${config.xdg.configHome}/git/config_gitlab"

          [includeIf "gitdir:${config.home.homeDirectory}/Desktop/personal/forgejo/"]
          	path = "${config.xdg.configHome}/git/config_forgejo"
          
          ${extraConfig}
        '';
      "git/config_gitlab".text =
        ''
          [user]
          	email = "${ssot.contact.gitlab.email}"
        '';
      "git/config_forgejo".text =
        ''
          [user]
          	email = "${ssot.contact.forgejo.email}"
        '';
    };
  };
}
