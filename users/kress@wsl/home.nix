{ username, hostname, ... }:
{ config, pkgs, lib, specialArgs, ... }: with specialArgs;
let
  # directories
  homeDirectory = "/home/${username}";

  # binaries
  fish = "${config.programs.fish.package}/bin/fish";
  direnv = "${pkgs.direnv}/bin/direnv";
  browser = "${pkgs.brave}/bin/brave";

  #
  # shared settings
  #

  gitExtraConfig = {
    user.name = "Chris";
    core.editor = "micro";
    rerere.enabled = true;
    pull.rebase = true;
    init.defaultBranch = "main";
  };

  defaultBrowser = "brave-browser.desktop";

  #
  # helpers
  #

  # TODO: move this to specialArgs as homeageFromConfig username hostname { identities, file }
  homeageFromSecrets = { identities, file }: {
      identityPaths = map (path: "${homeDirectory}/${path}") identities;
      installationType = "activation";
      file = lib.attrsets.mapAttrs' (target: source:
        {
          name = toString source;
          value = {
            source = secretPath username hostname source;
            copies = [ "${homeDirectory}/${target}" ];
          };
        }
      ) file;
  };

  toGitINI = compose [
    (lib.trivial.mergeAttrs gitExtraConfig)
    lib.generators.toGitINI
  ];

  secret = secretPath username hostname;
in
{
  imports = [ homeage.homeManagerModules.homeage ];

  home = { inherit homeDirectory username; };

  #
  # config files
  #

  home.file = {
    ".bashrc".source = home ".bashrc";
    ".bash_profile".source = home ".bash_profile";
    ".profile".text = ''
      if [[ $(ps --no-header --pid=$PPID --format=comm) != "fish" && -z "$BASH_EXECUTION_STRING" ]]
      then
        exec ${fish}
      fi
    '';
    "gitlab/.envrc".source = ./gitlab/.envrc;
    "gitlab/.gitconfig".text = toGitINI {
      user.email = "christian.ferraz@paack.co";
    };
    "paack/.envrc".source = ./paack/.envrc;
    "paack/.gitconfig".text = toGitINI {
      user.email = "664520-kress95@users.noreply.gitlab.com";
      user.name = "Christian Ferraz";
    };
  };

  xdg = {
    enable = true;

    dataFile = {
      "warp/accepted-teams-tos.txt".text = "yes\n";
      "warp/accepted-tos.txt".text = "yes\n";
    };

    # default apps
    mimeApps = {
      enable = true;
      defaultApplications = {
        "x-scheme-handler/http" = defaultBrowser;
        "x-scheme-handler/https" = defaultBrowser;
        "x-scheme-handler/chrome" = defaultBrowser;
        "text/html" = defaultBrowser;
        "application/x-extension-htm" = defaultBrowser;
        "application/x-extension-html" = defaultBrowser;
        "application/x-extension-shtml" = defaultBrowser;
        "application/xhtml+xml" = defaultBrowser;
        "application/x-extension-xhtml" = defaultBrowser;
        "application/x-extension-xht" = defaultBrowser;
      };
    };
  };

  homeage = homeageFromSecrets {
    identities = [ ".ssh/id_ed25519" ];
    file = {
      "paack/.secretrc" = "paack/.secretrc.age";
      ".envrc" = ".envrc.age";
      ".npmrc" = ".npmrc.age";
    };
  };

  #
  # programs
  #

  programs.git = {
    enable = true;
    userName = gitExtraConfig.user.name;
    userEmail = "2013206+kress95@users.noreply.github.com";
    extraConfig = gitExtraConfig;
  };

  programs.keychain = {
    enable = true;
    keys = [ "id_ed25519" ];
  };

  programs.fish = {
    enable = true;
    shellAliases = {
      "g" = "git";
      "sys" = "git --git-dir=$HOME/.system.git --work-tree=/etc/nixos";
      "rebuild-sys" = "sudo nixos-rebuild switch -v && rebuild-home -v";
      "rebuild-home" = "eval (cat /etc/systemd/system/home-manager-$USER.service | sed -n 's/ExecStart=//p')";
    };
    shellInit = ''
      set -g SHELL "${fish}"
      ${(builtins.readFile (misc "shell_init.fish"))}
    '';
    functions = {
      dev = builtins.readFile (home ".config/fish/functions/dev.fish");
    };
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  #
  # services
  #

  systemd.user.startServices = "sd-switch";

  #
  # activation scripts
  #

  home.activation = {
    direnvAllow = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      $DRY_RUN_CMD ${direnv} allow $HOME
      $DRY_RUN_CMD ${direnv} allow $HOME/gitlab
      $DRY_RUN_CMD ${direnv} allow $HOME/paack
    '';
  };

  # before changing this value read the documentation for this option
  home.stateVersion = "22.11";
}
