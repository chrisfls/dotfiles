{ config, lib, pkgs, specialArgs, ... }:
let
  ssot = specialArgs.ssot;
  eDP-1 = "00ffffffffffff0009e5030003000000011d0103800b11782f0000a057499b2610484f00000001010101010101010101010101010101c21a205030001050101032006cac00000018000000fc00545630383057554d2d4e4c300a000000fd003c3c101007000000000000000000000000000000000000000000000000000000c3";
  HDMI-1 = "00ffffffffffff004c2d17104a4a34591c200103803f24782ac8b5ad50449e250f5054bfef80714f810081c081809500a9c0b300010108e80030f2705a80b0588a0078682100001e000000fd00324b1e873c000a202020202020000000fc004c5532385235350a2020202020000000ff00485835543730303637370a2020019b020335f04961120313041f10605f2309070783010000e305c0006b030c001000b83c2000200167d85dc401788003e20f81e3060501023a801871382d40582c450078682100001e023a80d072382d40102c458078682100001e04740030f2705a80b0588a0078682100001e565e00a0a0a029503020350078682100001a000059";
in
{
  imports = [
    ../preset/non-nixos.nix
    ../preset/development.nix
    ../preset/development-desktop.nix
    ../preset/desktop.nix
    ../preset/work.nix
    ../preset/gamedev.nix
  ];

  extra.cloudflare-warp.enable = true;
  extra.scaling = { enable = true; scale = 1.5; };

  home.keyboard.layout = "br";

  programs.autorandr = {
    enable = true;
    profiles = {
      undocked = {
        fingerprint = { inherit eDP-1; };
        config = {
          DP-1.enable = false;
          DP-2.enable = false;
          eDP-1 = {
            enable = true;
            crtc = 0;
            primary = true;
            position = "0x0";
            mode = "800x1280";
            rate = "60.06";
            rotate = "right";
            # x-prop-aspect_ratio Automaticmn
            # x-prop-audio auto
            # x-prop-broadcast_rgb Automatic
            # x-prop-colorspace Default
            # x-prop-max_bpc 8
            # x-prop-non_desktop 0
          };
        };
      };
      docked = {
        fingerprint = { inherit eDP-1 HDMI-1; };
        config = {
          eDP-1.enable = false;
          DP-1.enable = false;
          DP-2.enable = false;
          HDMI-1 = {
            enable = true;
            crtc = 0;
            primary = true;
            position = "0x0";
            mode = "3840x2160";
            rate = "60.00";
            # x-prop-aspect_ratio Automatic
            # x-prop-audio auto
            # x-prop-broadcast_rgb Automatic
            # x-prop-colorspace Default
            # x-prop-max_bpc 8
            # x-prop-non_desktop 0
          };
        };
      };
    };
  };

  services.autorandr.enable = true;

  home.username = ssot.users.arch-rmxp.kress.username;
  home.homeDirectory = ssot.users.arch-rmxp.kress.home;

  # before changing this value read the documentation for this option
  home.stateVersion = "23.11";

  # let home manager install and manage itself
  programs.home-manager.enable = true;
}
