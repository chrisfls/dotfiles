{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.module.shell) enable;

  bin = "${config.programs.fish.package}/bin/fish";

  ssot = specialArgs.ssot;
in
{
  options.module.shell.enable = lib.mkEnableOption "Enable shell module";

  config = lib.mkIf enable {
    module.micro.enable = true;

    home.packages = [
      pkgs.jq
      pkgs.jaq
      pkgs.which
    ];

    # pacman.usr = {
    #   # BREAKAGE WARNING: 'bash' 'fish'
    #   direnv = [ "extra/direnv" ];
    #   git = [ "extra/git" ];
    #   jaq = [ "extra/jaq" ];
    #   jq = [ "extra/jq" ];
    #   keychain = [ "extra/keychain" ];
    #   which =  [ "core/which" ];
    #   zoxide = [ "extra/zoxide" ];
    # };

    home.sessionVariables = {
      EDITOR = "micro";
      TERM = "xterm-256color";
      COLORTERM = "truecolor";
      MICRO_TRUECOLOR = "1";
      VTE_VERSION = "6003";
      DIRENV_LOG_FORMAT = "";
    };

    programs.bash = {
      enable = true;
      enableVteIntegration = true;
      profileExtra = "unset HISTFILE";
      historyFileSize = 0;
      historySize = 0;
    };

    programs.fish = {
      enable = true;
      shellAliases."g" = "git";
      plugins = [
        { name = "autopair-fish"; src = pkgs.fishPlugins.autopair-fish.src; }
        { name = "colored-man-pages"; src = pkgs.fishPlugins.colored-man-pages.src; }
        { name = "foreign-env"; src = pkgs.fishPlugins.foreign-env.src; }
        { name = "sponge"; src = pkgs.fishPlugins.sponge.src; }
        { name = "tide"; src = pkgs.fishPlugins.tide.src; }
      ];
      shellInit =
        ''
          set -g SHELL "${bin}"

          set fish_greeting ""

          if not set -q tide_setup
            echo "tide setup init"
            echo 1 2 1 2 3 2 1 1 y | tide configure >/dev/null
            set --universal tide_character_icon "λ"
            set --universal tide_setup true
            echo "tide setup done"
          end
        '';
    };

    programs.keychain = {
      enable = true;
      keys = [ "id_ed25519" ];
    };

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

    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
      enableBashIntegration = true;
    };

    programs.zoxide = {
      enable = true;
      enableBashIntegration = true;
      enableFishIntegration = true;
    };
  };
}
