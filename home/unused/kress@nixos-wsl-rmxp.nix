{ config, pkgs, lib, specialArgs, ... }:
with specialArgs;
{
  imports = [
    ../../../../home
    ../../../../home/default-browser.nix
    ../../../../home/dev.nix
    ../../../../home/direnv.nix
    ../../../../home/fish
    ../../../../home/git.nix
    ../../../../home/homeage.nix
    ../../../../home/paack
    ../../../../home/warp.nix
    ../../../../home/wsl
  ];

  module = {
    default-browser = {
      enable = true;
      name = "chromium-browser.desktop";
    };
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
    paack.enable = true;
    warp.enable = true;
    wsl = {
      enable = true;
      vscode.enable = true;
    };
  };
  
  home.packages = with pkgs; [
    chromium
  ];

  programs.fish = {
    shellAliases = {
      "f" = "explorer";
      "rebuild-sys" = "sudo nixos-rebuild switch -v && rebuild-home -v";
      "rebuild-home" = "eval (cat /etc/systemd/system/home-manager-$USER.service | sed -n 's/ExecStart=//p')";
    };
  };

  # before changing this value read the documentation for this option
  home.stateVersion = "22.11";
}
