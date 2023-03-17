{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.module.zellij;
in
{
  options.module.zellij = {
    enable = mkEnableOption "zellij module";
    shell = mkOption {
      type = types.str;
      default = "fish";
    };
  };

  config = mkIf cfg.enable {
    home.file = {
      ".profile".text = ''
      export ZELLIJ_AUTO_EXIT="true"
      eval "$(zellij setup --generate-auto-start bash)"
      '';
    };

    programs.zellij = {
      enable = true;
      settings = {
        default_shell = cfg.shell;
        on_force_close = "quit";
        default_mode = "locked";
        keybinds = {
          unbind = [ { "Ctrl" = "q"; } ];
        };
        ui.pane_frames.rounded_corners = true;
        theme = "custom";
        themes.custom.fg = "#F2E5BC";
        themes.custom.bg = "#000000";
        themes.custom.black = "#1D2021";
        themes.custom.blue = "#458588";
        themes.custom.cyan = "#689D6A";
        themes.custom.green = "#B8BB26";
        themes.custom.magenta = "#B16286";
        themes.custom.orange = "#D25F1F";
        themes.custom.red = "#CC241D";
        themes.custom.white = "#A89984";
        themes.custom.yellow = "#D79921";
      };
    };
  };
}
