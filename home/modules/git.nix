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
          	email = "${ssot.contact.email}"
          	name = "${ssot.contact.name}"

          [includeIf "gitdir:${config.home.homeDirectory}/Desktop/personal/github/"]
          	path = "${config.xdg.configHome}/git/config_github"

          [includeIf "gitdir:${config.home.homeDirectory}/Desktop/personal/gitlab/"]
          	path = "${config.xdg.configHome}/git/config_gitlab"

          ${extraConfig}
        '';
      "git/config_github".text =
        ''
          [core]
          	sshCommand = "ssh -o PreferredAuthentications=publickey -i ~/.ssh/id_ed25519_personal -F /dev/null"

          [user]
          	email = "${ssot.contact.github.email}"
        '';
      "git/config_gitlab".text =
        ''
          [core]
          	sshCommand = "ssh -o PreferredAuthentications=publickey -i ~/.ssh/id_ed25519_personal -F /dev/null"

          [user]
          	email = "${ssot.contact.gitlab.email}"
        '';
    };
  };
}
