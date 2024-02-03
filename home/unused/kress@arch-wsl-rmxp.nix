{ config, pkgs, specialArgs, ... }: with specialArgs;
{
  imports = [
    ../../../../home
    ../../../../home/default-browser.nix
    ../../../../home/dev.nix
    ../../../../home/direnv.nix
    ../../../../home/fish
    ../../../../home/git.nix
    ../../../../home/homeage.nix
    ../../../../home/archlinux.nix
    ../../../../home/paack
    ../../../../home/warp.nix
    ../../../../home/wsl
  ];

  modules = {
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
    archlinux.enable = true;
    paack.enable = true;
    warp.enable = true;
    wsl.enable = true;
  };

  programs.fish = {
    shellAliases = {
      "f" = "explorer";
      "upgrade" = "sys-update && home-update";
      "sys-update" = "sudo pacman -Sy --needed --noconfirm archlinux-keyring chaotic-keyring && sudo powerpill -Su --noconfirm && sudo paccache -r";
      "home-update" = "home-manager switch --flake '/etc/nixos' && nix-env --delete-generations old && nix-store --gc";
      "cleanup" = "sudo paccache -r && nix-env --delete-generations old && nix-store --gc";
    };
  };

  # let home manager install and manage itself
  programs.home-manager.enable = true;
  
  # before changing this value read the documentation for this option
  home.stateVersion = "22.11";
}
