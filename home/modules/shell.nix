{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.modules.shell) enable;

  bin = "${config.programs.fish.package}/bin/fish";

  ssot = specialArgs.ssot;
in
{
  options.modules.shell.enable = lib.mkEnableOption "Enable shell module";

  config = lib.mkIf enable {
    programs.git = {
      enable = true;
      userName = ssot.contact.name;
      userEmail = ssot.contact.email;
      extraConfig = {
        rerere.enabled = true;
        pull.rebase = true;
        init.defaultBranch = "main";
        core.editor = "micro";
        core.excludesfile = "$NIXOS_CONFIG_DIR/scripts/gitignore";
      };
      includes = [
        {
          condition = "gitdir:${config.home.homeDirectory}/personal/";
          contents.user.name = ssot.contact.name;
          contents.user.email = ssot.contact.email;
        }
        {
          condition = "gitdir:${config.home.homeDirectory}/personal/gitlab/";
          contents.user.email = ssot.contact.gitlab.email;
        }
      ];
    };
  };
}
