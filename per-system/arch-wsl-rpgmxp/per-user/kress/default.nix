{ config, pkgs, specialArgs, ... }: with specialArgs;
{
  imports = [
    ../../../../home
    ../../../../home/dev.nix
    ../../../../home/direnv.nix
    ../../../../home/fish
    ../../../../home/git.nix
    ../../../../home/homeage.nix
    ../../../../home/non-nixos.nix
    ../../../../home/paack
    ../../../../home/warp.nix
    ../../../../home/wsl
  ];

  module = {
    dev.enable = true;
    direnv.enable = true;
    fish = {
      enable = true;
      wsl = {
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
        "paack/.secretrc" = "${config.home.username}/paack/.secretrc.age";
        ".envrc" = "${config.home.username}/.envrc.age";
        ".npmrc" = "${config.home.username}/.npmrc.age";
      };
    };
    non-nixos.enable = true;
    paack.enable = true;
    warp.enable = true;
    wsl.enable = true;
  };

  # let home manager install and manage itself
  programs.home-manager.enable = true;
  
  # before changing this value read the documentation for this option
  home.stateVersion = "22.11";
}