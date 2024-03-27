{ config, lib, pkgs, specialArgs, ... }:
let
  ssot = specialArgs.ssot;
  eDP-1 = "00ffffffffffff0009e5030003000000011d0103800b11782f0000a057499b2610484f00000001010101010101010101010101010101c21a205030001050101032006cac00000018000000fc00545630383057554d2d4e4c300a000000fd003c3c101007000000000000000000000000000000000000000000000000000000c3";
  HDMI-1 = "00ffffffffffff004c2d17104a4a34591c200103803f24782ac8b5ad50449e250f5054bfef80714f810081c081809500a9c0b300010108e80030f2705a80b0588a0078682100001e000000fd00324b1e873c000a202020202020000000fc004c5532385235350a2020202020000000ff00485835543730303637370a2020019b020335f04961120313041f10605f2309070783010000e305c0006b030c001000b83c2000200167d85dc401788003e20f81e3060501023a801871382d40582c450078682100001e023a80d072382d40102c458078682100001e04740030f2705a80b0588a0078682100001e565e00a0a0a029503020350078682100001a000059";
in
{
  presets = {
    desktop = true;
    development = true;
    gamedev = true;
    stream = true;
    work = true;
  };

  modules = {
    cloudflare-warp.enable = true;

    onedrive.enable = true;

    retroarch.enable = true;
  };

  pacman = {
    enable = true;
    packages = [
      "extra/intel-ucode"
      # "extra/blueman"
    ];
  };

  nixpkgs.config.permittedInsecurePackages = [
    "electron-25.9.0"
    "openssl-1.1.1w"
  ];

  homeage = {
    identityPaths = [ ".ssh/id_ed25519" ];
    installationType = "systemd";
  };

  home.keyboard.layout = "br";

  home.packages = [
    (pkgs.writeHostScriptBin "upgrade"
      ''
        system-switch && home-switch
      '')

    (pkgs.writeHostScriptBin "system-switch"
      ''
        pacman-switch && sudo paccache -r
      '')

    (pkgs.writeHostScriptBin "home-switch"
      ''
        home-manager switch && nix-env --delete-generations old && nix-store --gc
      '')

    (pkgs.writeHostScriptBin "cleanup"
      ''
        sudo paccache -r && nix-env --delete-generations old && nix-store --gc
      '')
  ];

  home.username = ssot.users.arch-rmxp.chris.username;
  home.homeDirectory = ssot.users.arch-rmxp.chris.home;

  # before changing this value read the documentation for this option
  home.stateVersion = "23.11";

  # let home manager install and manage itself
  programs.home-manager.enable = true;
}
