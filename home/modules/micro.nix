{ config, lib, pkgs, ... }:
let inherit (config.modules.micro) enable desktop; in {
  # TODO: pacman

  options.modules.micro.enable = lib.mkEnableOption "Enable micro module";
  options.modules.micro.desktop = lib.mkEnableOption "Enable micro desktop entry";

  config = lib.mkIf enable {
    pacman.pkgs.micro = [ "extra/micro" ];

    programs.micro = {
      enable = true;
      settings.colorscheme = "gruvbox-tc";
    };

    xdg.desktopEntries."micro" = lib.mkIf desktop {
      name = "Micro";
      genericName = "Text Editor";
      comment = "Edit text files in a terminal";
      icon = "micro";
      type = "Application";
      categories = [ "Utility" "TextEditor" "Development" ];
      exec = "micro %F";
      startupNotify = true;
      terminal = true;
      noDisplay = false;
      mimeType = [
        "text/plain"
        "text/x-chdr"
        "text/x-csrc"
        "text/x-c++hdr"
        "text/x-c++src"
        "text/x-java"
        "text/x-dsrc"
        "text/x-pascal"
        "text/x-perl"
        "text/x-python"
        "application/x-php"
        "application/x-httpd-php3"
        "application/x-httpd-php4"
        "application/x-httpd-php5"
        "application/xml"
        "text/html"
        "text/css"
        "text/x-sql"
        "text/x-diff"
      ];
    };
  };
}
