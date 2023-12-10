{ config, pkgs, lib, specialArgs, flakes, ... }:
with specialArgs;
{
  programs.nix-index.enable = true;

  programs.fish =
    let
      bin = "${config.programs.fish.package}/bin/fish";
    in
    {
      enable = true;
      shellAliases = {
        "ns" = "nix search nixpkgs";
        "g" = "git";
      };
      plugins = [
        { name = "autopair-fish"; src = pkgs.fishPlugins.autopair-fish.src; }
        { name = "colored-man-pages"; src = pkgs.fishPlugins.colored-man-pages.src; }
        { name = "foreign-env"; src = pkgs.fishPlugins.foreign-env.src; } # probably not needed
        { name = "sponge"; src = pkgs.fishPlugins.sponge.src; }
        { name = "tide"; src = pkgs.fishPlugins.tide.src; }
      ];
      shellInit =
        ''
          set -g SHELL "${bin}"
          ${(builtins.readFile ./shell_init.fish)}
        '';
    };

  programs.bash = {
    enable = true;
    enableVteIntegration = true;
    sessionVariables = {
      EDITOR = "micro";
      TERM = "xterm-256color";
      COLORTERM = "truecolor";
      MICRO_TRUECOLOR = "1";
      VTE_VERSION = "6003";
      DIRENV_LOG_FORMAT = "";
      PATH = "$PATH:$HOME/.nix-profile/bin";
    };
    profileExtra = ''
      unset HISTFILE
    '';
    historyFileSize = 0;
    historySize = 0;
  };

  programs.micro = {
    enable = true;
    settings = {
      colorscheme = "gruvbox-tc";
    };
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
      core = {
        editor = "micro";
        excludesfile = "$NIXOS_CONFIG_DIR/scripts/gitignore";
      };
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

  home.packages = with pkgs; [
    # tools
    jq
    wget
    which

    # nix
    any-nix-shell
    cachix
    flakes.devenv.packages.${pkgs.system}.devenv
    nixpkgs-fmt

    # edit secrets
    flakes.agenix.packages.${pkgs.system}.default

    # languagetool
    ltex-ls
    adoptopenjdk-jre-openj9-bin-16
  ];


  # (patchDesktop vscodium "d" "\^Exec=\\\\(.\*\\\\)Discord" "Exec=\\\\1Discord --enable-features=UseOzonePlatform --ozone-platform=wayland")

  programs.vscode = {
    enable = true;
    package = pkgs.vscodium;
    extensions = with pkgs.vscode-extensions; [
      editorconfig.editorconfig
      waderyan.gitblame
      bbenoist.nix
      b4dm4n.vscode-nixpkgs-fmt
    ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
      {
        name = "yuck";
        publisher = "eww-yuck";
        version = "0.0.3";
        sha256 = "sha256-DITgLedaO0Ifrttu+ZXkiaVA7Ua5RXc4jXQHPYLqrcM=";
      }
      {
        
        name = "default-keys-windows";
        publisher = "smcpeak";
        version = "0.0.10";
        sha256 = "sha256-v1JY5ZGWOfF14H235Y9CLlPwIvmNwCeRhIkdmcgCCFU=";
      }
    ];
  };

  home.activation =
    let
      bin = "${pkgs.direnv}/bin/direnv";
      direnvAllow = (path: "$DRY_RUN_CMD sh -c 'if [ -f \"${path}/.envrc\" ]; then ${bin} allow \"${path}\"; fi;'");
    in
    {
      direnvAllowHome = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        ${direnvAllow "$HOME"} 
      '';
    };

  # cloudflare warp
  xdg = {
    enable = true;
    dataFile = {
      "warp/accepted-teams-tos.txt".text = "yes\n";
      "warp/accepted-tos.txt".text = "yes\n";
    };
    configFile = {
      "electron25-flags.conf".text = ''
        --enable-features=WaylandWindowDecorations
        --ozone-platform-hint=auto
      '';
    };
  };
  
  
}
