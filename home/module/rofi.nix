{ config, lib, pkgs, ... }:
let
  cfg = config.module.rofi;

  rofi = pkgs.fetchFromGitHub {
    owner = "kress95";
    repo = "rofi";
    rev = "896d5dea410618701bf6c5d81a804e213a08b571";
    sha256 = "sha256-RwFPqZSe/YJBTpAxmdfcsRbpgIYDUba6nqEUT6xy8ZE=";
  };

  dpi = builtins.toString (builtins.floor config.module.scaling.dpiScaled);

  pkg = pkgs.symlinkJoin {
    name = "rofi";
    paths = [
      (pkgs.writeShellScriptBin "rofi" "${pkgs.rofi}/bin/rofi -dpi ${dpi} $@")
      pkgs.rofi
    ];
  };

  user = config.home.username;
in
{
  options.module.rofi.enable = lib.mkEnableOption "Enable rofi module";

  config = lib.mkIf cfg.enable {
    home.packages = [ pkg pkgs.hostname ];

    services.sxhkd.keybindings."super + Return" = "rofi -show drun -theme \"${config.module.themes.rofi}\"";

    xdg.dataFile = {
      "fonts/GrapeNuts-Regular.ttf".source = "${rofi}/fonts/GrapeNuts-Regular.ttf";
      "fonts/Icomoon-Feather.ttf".source = "${rofi}/fonts/Icomoon-Feather.ttf";
      # "fonts/Iosevka-Nerd-Font-Complete.ttf".source = "${rofi}/fonts/Iosevka-Nerd-Font-Complete.ttf";
      # "fonts/JetBrains-Mono-Nerd-Font-Complete.ttf".source = "${rofi}/fonts/JetBrains-Mono-Nerd-Font-Complete.ttf";
    };

    xdg.configFile."rofi".source = "${rofi}/files";
  };
}
