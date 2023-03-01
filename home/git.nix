{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.module.git;
in
{
  imports = [
    ./micro.nix
    ./keychain.nix
  ];

  options.module.git = {
    enable = mkEnableOption "git module";
  };

  config = mkIf cfg.enable {
    module.micro.enable = true;
    module.keychain.enable = true;
    
    programs.git = {
      enable = true;
      userName = "Chris";
      userEmail = "2013206+kress95@users.noreply.github.com";
      extraConfig = {
        rerere.enabled = true;
        pull.rebase = true;
        init.defaultBranch = "main";
        core = {
          editor = "micro";
          excludesfile = "$NIXOS_CONFIG_DIR/scripts/gitignore";
        };
      };
      includes = [
        {
          condition = "gitdir:${config.home.homeDirectory}/gitlab/";
          contents.user.email = "664520-kress95@users.noreply.gitlab.com";
        }
        {
          condition = "gitdir:${config.home.homeDirectory}/paack/";
          contents = {
            user.name = "Christian Ferraz";
            user.email = "christian.ferraz@paack.co";
          };
        }
      ];
    };
  };
}
