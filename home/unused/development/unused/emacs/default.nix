# I prefer using kruft to using `nix-community/nix-doom-emacs` for the
# following reasons:
#
# 1. nix-doom-emacs rebuilds everything each time I change settings.
# 2. nix-doom-emacs doesn't fetch packages from github by default,
#    so I'd have to sync my packages.el with this file.
# 3. I don't need to rebuild my home each time I change settings [1].
{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.modules.emacs;
  emacs = "${config.home.homeDirectory}/.config/emacs";
  # doom = "${emacs}/bin/doom";
in
{
  options.modules.emacs = {
    enable = mkEnableOption "emacs module";
    # doomRev = mkOption {
    #   type = types.str;
    #   default = "22097b5a755a5b1d661e362a8441b61e37f777c9";
    # };
  };

  config = mkIf cfg.enable {
    programs.emacs = {
      enable = true;
      package = pkgs.emacs-gtk;
    };

    fonts.fontconfig.enable = true;

    home.packages = with pkgs; [
      jetbrains-mono
      ripgrep
      fd
      enchant # (spell +enchant)
      wordnet # (lookup +offline)
      sqlite # (magit +forge)
    ];

    xdg = {
      enable = true;
      configFile  = {
        "emacs".source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/home/emacs/custom;
        # [1] If I ever stop using `mkOutOfStoreSymlink` this reason is out.
        # "doom".source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/home/emacs/doom;
      };
    };
  
    programs.fish.shellAliases = {
      "emacsb" = "emacs --build";
      # "doom" = doom;
    };
    
    # home.activation = {
    #   # ohgodwhy
    #   doomSetup = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    #     $DRY_RUN_CMD export PATH="$PATH:${pkgs.emacs}/bin:${pkgs.git}/bin"
    #     $DRY_RUN_CMD sh -c 'if [ ! -d "${emacs}" ]; then git clone https://github.com/doomemacs/doomemacs.git "${emacs}"; fi;'
    #     $DRY_RUN_CMD git -C "${emacs}" checkout ${cfg.doomRev}
    #     $DRY_RUN_CMD ${doom} install -! --no-config --no-hooks
    #     $DRY_RUN_CMD ${doom} sync
    #   '';
    # };
  };
}
