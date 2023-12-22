{ config, lib, pkgs, ... }:
let
  cfg = config.extra.picom;
in
{
  options.extra.picom.enable = lib.mkEnableOption "Enable picom module";

  config = lib.mkIf cfg.enable {
    services.picom = {
      enable = true;
      package = pkgs.picom-next;
      backend = "glx";
      extraArgs = [
        "--vsync-use-glfinish"
        "--glx-no-stencil"
        "--no-use-damage"
      ];
      vSync = true;
      settings = {
        blur = {
          method = "dual_kawase";
          strength = 8;
        };
      };
      wintypes = {
        normal = { blur-background = true; };
        splash = { blur-background = false; };
      };
    };
  };
}
