{ config, lib, pkgs , ... }:
let
  xorg = pkgs.xorg;
  i3 = config.xsession.windowManager.i3;
  cfg = config.xsession.windowManager.i3.config;
  mod = cfg.modifier;
in
{
  home.packages = [
    pkgs.autotiling
    pkgs.picom
  ];
  
  home.file.".xinitrc".text = "exec ${i3.package}/bin/i3";

  xsession.windowManager.i3 = {
    enable = true;

    config = {

      startup = [
        # { command = "autotiling"; always = true; notification = false; }
        { command = "nixGLIntel picom"; always = true; notification = false; }
      ];

      modifier = "Mod4";

      ################# ######## #### ## #
      # BINDINGS
      ################# ######## #### ## #

      keybindings = {

        # MISC
        ######## #### ## #

        "${mod}+Shift+c" = "reload";
        "${mod}+Shift+r" = "restart";
        "${mod}+Shift+e" =
          "exec i3-nagbar -t warning -m 'Do you want to exit i3?' -b 'Yes' 'i3-msg exit'";

        # APPLICATIONS
        ######## #### ## #23

        "${mod}+colon" = "exec ${cfg.terminal}";
        "${mod}+BackSpace" = "exec ${cfg.terminal}";
        "${mod}+Return" = "exec ${cfg.menu}";

        # WINDOW CONTROLS
        ######## #### ## #

        "${mod}+c" = "kill";

        "${mod}+r" = "mode resize";

        "${mod}+m" = "fullscreen toggle";

        "${mod}+s" = "layout toggle split";

        "${mod}+g" = "layout tabbed";

        "${mod}+f" = "floating toggle";

        "${mod}+space" = "focus mode_toggle";

        #"${mod}+h" = "split h";
        "${mod}+v" = "split toggle";

        "${mod}+a" = "focus parent";

        # FOCUS
        ######## #### ## #

        # focus with hjkl
        "${mod}+h" = "focus left";
        "${mod}+j" = "focus down";
        "${mod}+k" = "focus up";
        "${mod}+l" = "focus right";

        # focus with arrow keys
        "${mod}+Left" = "focus left";
        "${mod}+Down" = "focus down";
        "${mod}+Up" = "focus up";
        "${mod}+Right" = "focus right";

        # MOVEMENT
        ######## #### ## #

        # move with hjkl
        "${mod}+Shift+h" = "move left";
        "${mod}+Shift+j" = "move down";
        "${mod}+Shift+k" = "move up";
        "${mod}+Shift+l" = "move right";

        # move with arrow keys
        "${mod}+Shift+Left" = "move left";
        "${mod}+Shift+Down" = "move down";
        "${mod}+Shift+Up" = "move up";
        "${mod}+Shift+Right" = "move right";

        # WORKSPACES
        ######## #### ## #

        # switch to workspace
        "${mod}+1" = "workspace number 1";
        "${mod}+2" = "workspace number 2";
        "${mod}+3" = "workspace number 3";
        "${mod}+4" = "workspace number 4";
        "${mod}+5" = "workspace number 5";
        "${mod}+6" = "workspace number 6";
        "${mod}+7" = "workspace number 7";
        "${mod}+8" = "workspace number 8";
        "${mod}+9" = "workspace number 9";
        "${mod}+0" = "workspace number 10";

        # switch to workspace
        "${mod}+Shift+1" = "move container to workspace number 1";
        "${mod}+Shift+2" = "move container to workspace number 2";
        "${mod}+Shift+3" = "move container to workspace number 3";
        "${mod}+Shift+4" = "move container to workspace number 4";
        "${mod}+Shift+5" = "move container to workspace number 5";
        "${mod}+Shift+6" = "move container to workspace number 6";
        "${mod}+Shift+7" = "move container to workspace number 7";
        "${mod}+Shift+8" = "move container to workspace number 8";
        "${mod}+Shift+9" = "move container to workspace number 9";
        "${mod}+Shift+0" = "move container to workspace number 10";

        # control scratchpad
        "${mod}+apostrophe" = "scratchpad show";
        "${mod}+Shift+apostrophe" = "move scratchpad";
      };

      modes = {

        # RESIZE
        ######## #### ## #

        resize = {
          # resize with hjkl
          "h" = "resize shrink width 10 px or 10 ppt";
          "j" = "resize grow height 10 px or 10 ppt";
          "k" = "resize shrink height 10 px or 10 ppt";
          "l" = "resize grow width 10 px or 10 ppt";

          # resize with arrow keys
          "Left" = "resize shrink width 10 px or 10 ppt";
          "Down" = "resize grow height 10 px or 10 ppt";
          "Up" = "resize shrink height 10 px or 10 ppt";
          "Right" = "resize grow width 10 px or 10 ppt";

          # quit resize
          "Escape" = "mode default";
          "Return" = "mode default";
          "${mod}+r" = "mode default";
        };
      };
    };
  };
}
