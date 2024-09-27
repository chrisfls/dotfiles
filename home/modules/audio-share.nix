{ config, lib, pkgs, ... }:
let inherit (config.modules.audio-share) enable; in {
  options.modules.audio-share.enable = lib.mkEnableOption "Enable audio-share module";

  config = lib.mkIf enable {
    pacman.packages = [ "aur/audio-share-bin" ];

    home.packages = [
      (pkgs.writeHostScriptBin "audio-share" (lib.readFile ../../assets/audio-share.sh))
    ];
  };
}
