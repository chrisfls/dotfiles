{ config, lib, pkgs, ... }:
let
  cfg = config.extra.rofi;

  rofi = pkgs.fetchFromGitHub {
    owner = "kress95";
    repo = "rofi";
    rev = "0a55e154598466edb4352013798acc1d2f245306";
    sha256 = "sha256-YjyrxappcLDoh3++mtZqCyxQV2qeoNhhUy2XGwlyTng=";
  };

  dpi = builtins.toString (builtins.floor config.extra.scaling.dpiScaled);

  pkg = pkgs.symlinkJoin {
    name = "rofi";
    paths = [
      (pkgs.writeShellScriptBin "rofi" "${pkgs.rofi}/bin/rofi -dpi ${dpi} $@")
      pkgs.rofi
    ];
  };
in
{
  options.extra.rofi.enable = lib.mkEnableOption "Enable rofi module";

  config = lib.mkIf cfg.enable {
    home.packages = [ pkg pkgs.hostname ];

    services.sxhkd.keybindings."super + Return" = "rofi -show drun -theme \"${config.xdg.configHome}/rofi/launchers/type-3/style-1.rasi\"";

    xdg.dataFile = {
      "fonts/GrapeNuts-Regular.ttf".source = "${rofi}/fonts/GrapeNuts-Regular.ttf";
      "fonts/Icomoon-Feather.ttf".source = "${rofi}/fonts/Icomoon-Feather.ttf";
      "fonts/Iosevka-Nerd-Font-Complete.ttf".source = "${rofi}/fonts/Iosevka-Nerd-Font-Complete.ttf";
      "fonts/JetBrains-Mono-Nerd-Font-Complete.ttf".source = "${rofi}/fonts/JetBrains-Mono-Nerd-Font-Complete.ttf";
    };

    # xresources.properties."rofi.dpi" = config.extra.scaling.dpiScaled;

    xdg.configFile."rofi".source = "${rofi}/files";
  };
}
