{ config, lib, pkgs, ... }:
let
  cfg = config.extra.rofi;

  rofi = pkgs.fetchFromGitHub {
    owner = "kress95";
    repo = "rofi";
    rev = "0a55e154598466edb4352013798acc1d2f245306";
    sha256 = "sha256-YjyrxappcLDoh3++mtZqCyxQV2qeoNhhUy2XGwlyTng=";
  };
in
{
  options.extra.rofi.enable = lib.mkEnableOption "Enable rofi module";

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.rofi ];

    services.sxhkd.keybindings."super + Return" = "rofi -show drun";

    xdg.dataFile = {
      "fonts/GrapeNuts-Regular.ttf".source = "${rofi}/fonts/GrapeNuts-Regular.ttf";
      "fonts/Icomoon-Feather.ttf".source = "${rofi}/fonts/Icomoon-Feather.ttf";
      "fonts/Iosevka-Nerd-Font-Complete.ttf".source = "${rofi}/fonts/Iosevka-Nerd-Font-Complete.ttf";
      "fonts/JetBrains-Mono-Nerd-Font-Complete.ttf".source = "${rofi}/fonts/JetBrains-Mono-Nerd-Font-Complete.ttf";
    };

    xdg.configFile."rofi".source = "${rofi}/files";
  };
}
