{ config, lib, pkgs, ... }:
let
  inherit (config.modules.mopidy) enable;

  python-pkgs = ps: with ps; [
  ];
in
{
  options.modules.mopidy.enable = lib.mkEnableOption "Enable mopidy module";

  config = lib.mkIf enable {

    services.mopidy = {
      enable = true;

      extensionPackages = [
        pkgs.mopidy-iris
        pkgs.mopidy-youtube
        pkgs.mopidy-local
      ];

      settings = {
        core = {
          cache_dir = "${config.xdg.cacheHome}/mopidy";
          config_dir = "${config.xdg.configHome}/mopidy";
          data_dir = " ${config.xdg.dataHome}/mopidy";
          max_tracklist_length = 10000;
          restore_state = false;
        };
        audio = {
          output = "pulsesink server=127.0.0.1:4713";
        };
        file = {
          media_dirs = [
            config.xdg.userDirs.music
          ];
          follow_symlinks = true;
          excluded_file_extensions = [
            ".html"
            ".zip"
            ".jpg"
            ".jpeg"
            ".png"
          ];
        };
        youtube = {
          enabled = true;
          channel_id = "UCo2VpoSR5J-sallyS1jrjLg";
          musicapi_enabled = true;
          allow_cache = true;
          autoplay_enabled = true;
          musicapi_browser_authentication_file = "${config.xdg.configHome}/mopidy/auth";
        };
      };
    };
  };
}
