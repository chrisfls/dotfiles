{ config, pkgs, lib, specialArgs, ... }:
with specialArgs;
{
  imports = [
    homeage.homeManagerModules.homeage
    ../../home/browser.nix
    ../../home/direnv.nix
    ../../home/fish
    ../../home/git.nix
    ../../home/home.nix
    ../../home/homeage.nix
    ../../home/warp.nix
    ../../home/wsl
  ];

  module = {
    browser = {
      enable = true;
      name = "brave-browser.desktop";
    };
    direnv = {
      enable = true;
      bin = "${pkgs.direnv}/bin/direnv";
    };
    fish = {
      enable = true;
      bin = "${config.programs.fish.package}/bin/fish";
      wsl =
        {
          enable = true;
          desktop = "/mnt/d/Users/kress/Desktop";
          windir = "/mnt/c/Windows";
        };
    };
    git.enable = true;
    home = {
      enable = true;
      username = "kress";
    };
    homeage = {
      enable = true;
      identities = [ ".ssh/id_ed25519" ];
      file = {
        "paack/.secretrc" = "paack/.secretrc.age";
        ".envrc" = ".envrc.age";
        ".npmrc" = ".npmrc.age";
      };
    };
    warp.enable = true;
    wsl = {
      enable = true;
      vscode.enable = true;
    };
  };

  home =
    {
      file = {
        "gitlab/.keep".text = "";
        "paack/.envrc".source = ./kress/paack/.envrc;
      };
      sessionPath = [
        "$HOME/.local/bin"
      ];
    };

  programs.fish = {
    shellAliases = {
      "f" = "explorer";
      "rebuild-sys" = "sudo nixos-rebuild switch --override-input nix-secrets /etc/nixos/secrets -v && rebuild-home -v";
      "rebuild-home" = "eval (cat /etc/systemd/system/home-manager-$USER.service | sed -n 's/ExecStart=//p')";
    };
  };

  # before changing this value read the documentation for this option
  home.stateVersion = "22.11";
}
