{ username, hostname, ... }@userArgs:
{ config, pkgs, lib, specialArgs, ... }: with specialArgs;
let
  # directories
  homeDirectory = "/home/${username}";
  windowsDataRoot = "/mnt/d"; # d is for data
  windowsHomeDirectory = "${windowsDataRoot}/Users/${username}";
  windowsDesktopDirectory = "${windowsHomeDirectory}/Desktop";
  windowsMainRoot = "/mnt/c"; # c is for "can't move out of here"
  windowsMainDirectory = "${windowsMainRoot}/Windows";
  windowsSystem32Directory = "${windowsMainDirectory}/System32";

  # binaries
  fish = "${config.programs.fish.package}/bin/fish";
  direnv = "${pkgs.direnv}/bin/direnv";
  browser = "${pkgs.brave}/bin/brave";

  #
  # shared settings
  #

  defaultBrowser = "brave-browser.desktop";

  #
  # helpers
  #

  homeageConfig = homeageConfigUser userArgs;
in
{
  imports = [ homeage.homeManagerModules.homeage ];

  home = { inherit homeDirectory username; };

  #
  # config files
  #

  home.file = {
    ".bashrc".source = fileFromHome ".bashrc";
    ".bash_profile".source = fileFromHome ".bash_profile";
    ".profile".text = ''
      if [[ $(ps --no-header --pid=$PPID --format=comm) != "fish" && -z "$BASH_EXECUTION_STRING" ]]
      then
        exec ${fish}
      fi
    '';
    "gitlab/.keep".text = "";
    "paack/.envrc".source = ./paack/.envrc;
  };

  home.sessionPath = [
    "$HOME/.local/bin"
  ];

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

  homeage = homeageConfig {
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
        condition = "gitdir:${homeDirectory}/gitlab/";
        contents.user.email = "664520-kress95@users.noreply.gitlab.com";
      }
      {
        condition = "gitdir:${windowsDesktopDirectory}/gitlab/";
        contents.user.email = "664520-kress95@users.noreply.gitlab.com";
      }
      {
        condition = "gitdir:${homeDirectory}/paack/";
        contents = {
          user.name = "Christian Ferraz";
          user.email = "christian.ferraz@paack.co";
        };
      }
    ];
  };

  programs.keychain = {
    enable = true;
    keys = [ "id_ed25519" ];
  };

  programs.fish = {
    enable = true;
    shellAliases = {
      "m" = "deno run -A Makefile.ts";
      "e" = "code";
      "f" = "explorer";
      "da" = "direnv allow";
      "g" = "git";
      "ns" = "nix search nixpkgs";
      # system management
      "s" = "git --git-dir=$HOME/.system.git --work-tree=/etc/nixos";
      "rebuild-home" = "eval (cat /etc/systemd/system/home-manager-$USER.service | sed -n 's/ExecStart=//p')";
      "rebuild-sys" = "sudo nixos-rebuild switch -v && rebuild-home -v";
      # wsl apps 
      "cmd" = "${windowsSystem32Directory}/cmd.exe"; # you can't run cmd inside a wsl folder on windows
      "neovide" = " /mnt/c/Users/kress/scoop/shims/neovide.exe --wsl";
      "powershell" = "${windowsSystem32Directory}/WindowsPowerShell/v1.0/powershell.exe";
    };
    shellInit = ''
      set -g SHELL "${fish}"
      set -g w "${windowsDesktopDirectory}/"
      ${(builtins.readFile (fileFromMisc "shell_init.fish"))}
    '';
    functions = {
      shell = ''
        # prefer using direnv as using nix-shell directly is slow
        # also depends on any-nix-shell to maintain current shell
        nix-shell shell.nix $argv[1..-1]
      '';
      develop = ''
        nix develop "/etc/nixos/shells/$argv[1]" -c fish $argv[2..-1];
      '';
      code = ''
        if count $argv >/dev/null
          set here (realpath -m $argv[1])
        else
          set here $PWD
        end
        if test -d $here
          powershell "code --folder-uri=vscode-remote://ssh-remote+localhost$here"
        else
          powershell "code --file-uri=vscode-remote://ssh-remote+localhost$here"
        end
      '';
      explorer = ''
        if count $argv >/dev/null
          set here (realpath -m $argv[1])
        else
          set here $PWD
        end
        # TODO: check if $here is a drive letter
        set here (string replace -a '/' '\\' "//wsl.localhost/NixOS$here")
        ${windowsMainDirectory}/explorer.exe $here
      '';
    };
  };

  programs.micro = {
    enable = true;
    settings = {
      colorscheme = "solarized-tc";
    };
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.nix-index.enable = true;

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
      $DRY_RUN_CMD ${direnv} allow $HOME/paack
    '';
  };

  # before changing this value read the documentation for this option
  home.stateVersion = "22.11";
}
