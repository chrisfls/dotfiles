{ config, lib, pkgs, ... }:
let inherit (config.modules.retroarch) enable; in
{
  options.modules.retroarch.enable = lib.mkEnableOption "Enable retroarch module";
  config = lib.mkIf enable {
    pacman.packages = [
      "extra/libretro-beetle-psx-hw"
      "extra/libretro-beetle-psx"
      "extra/libretro-core-info"
      "extra/libretro-desmume"
      "extra/libretro-flycast"
      "extra/libretro-genesis-plus-gx"
      "extra/libretro-kronos"
      "extra/libretro-melonds"
      "extra/libretro-mesen-s"
      "extra/libretro-mesen"
      "extra/libretro-mgba"
      "extra/libretro-mupen64plus-next" # git version available
      "extra/libretro-parallel-n64"
      "extra/libretro-pcsx2"
      "extra/libretro-play"
      "extra/libretro-ppsspp"
      "extra/libretro-sameboy"
      "extra/libretro-shaders-slang"
      "extra/libretro-yabause"
      "extra/retroarch-assets-ozone"
      "extra/retroarch"
    ];
  };
}
