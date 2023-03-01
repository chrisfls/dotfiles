{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.module.warp;
in
{
  imports = [
    ./default-browser.nix
  ];

  options.module.warp = {
    enable = mkEnableOption "warp module";
  };

  config = mkIf cfg.enable {
    module.default-browser = {
      enable = true;
      name = "chromium-browser.desktop";
    };

    xdg = {
      enable = true;
      dataFile = {
        "warp/accepted-teams-tos.txt".text = "yes\n";
        "warp/accepted-tos.txt".text = "yes\n";
      };
    };

    home.packages = with pkgs; [
      chromium
      xdg-utils
      desktop-file-utils
    ];
  };
}
