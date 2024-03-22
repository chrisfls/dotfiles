{ config, lib, ... }:
let inherit (config.modules.code) enable; in {
  options.modules.code.enable = lib.mkEnableOption "Enable code module";

  config = lib.mkIf enable {
    pacman.packages = [ "chaotic-aur/visual-studio-code-bin" ];

    /*xdg.desktopEntries."code" = {
      name = "Visual Studio Code";
      comment = "Code Editing. Refined.";
      genericName = "Text Editor";
      exec = "/usr/bin/code --enable-features=UseOzonePlatform --ozone-platform=wayland --enable-features=WaylandWindowDecorations --unity-launch %F";
      icon = "visual-studio-code";
      type = "Application";
      startupNotify = false;
      categories = [ "TextEditor" "Development" "IDE" ];
      mimeType = [
        "text/plain"
        "inode/directory"
        "application/x-code-workspace"
      ];
      actions."new-empty-window" = {
        name = "New Empty Window";
        exec = "/usr/bin/code --enable-features=UseOzonePlatform --ozone-platform=wayland --enable-features=WaylandWindowDecorations --new-window %F";
        icon = "visual-studio-code";
      };
    };*/
  };
}
