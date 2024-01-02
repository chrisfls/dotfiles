{ config, lib, pkgs, ... }:
let
  cfg = config.module.screenshot;

  shotgun = "${pkgs.shotgun}/bin/shotgun"; # or pkgs.maim
  slop = "${pkgs.slop}/bin/slop"; # or pkgs.hacksaw
  xclip = "${pkgs.xclip}/bin/xclip";

  screenshot-copy = pkgs.writeShellScriptBin "screenshot-copy"
    ''
      ${shotgun} - | ${xclip} -t 'image/png' -selection clipboard
    '';

  screenshot-copy-area = pkgs.writeShellScriptBin "screenshot-copy-area"
    ''
      ${shotgun} -g "$(${slop} -r guides)" - | ${xclip} -t 'image/png' -selection clipboard
    '';

  screenshot-save = pkgs.writeShellScriptBin "screenshot-save"
    ''
      d="$XDG_PICTURES_DIR/Screenshots/$(date +"%Y-%m-%d")"
      mkdir -p "$d"
      ${shotgun} "$d/$(date +"%H_%M_%S.png")"
    '';
in
{
  options.module.screenshot.enable = lib.mkEnableOption "Enable screenshot module";

  config = lib.mkIf cfg.enable {
    home.packages = [
      screenshot-copy
      screenshot-copy-area
      screenshot-save
    ];

    xdg = {
      enable = true;
      configFile = {
        "slop/guides.vert".text =
          ''
            #version 130

            in vec2 position;
            in vec2 uv;

            out vec2 uvCoord;

            void main()
            {
                uvCoord = uv;
                gl_Position = vec4(position, 0, 1);
            }
          '';

        "slop/guides.frag".text =
          ''
            #version 130

            // inputs/outputs
            uniform sampler2D texture;
            uniform vec2 screenSize;
            uniform vec2 mouse;

            in vec2 uvCoord;

            out vec4 outColour;

            // configurables
            uniform vec4 colours[2] = vec4[2](
                vec4(1, 1, 1, 1),
                vec4(0.375, 0.375, 0.375, 1)
            );
            uniform float ant_size = 2;

            int mod_i(float n, int m) {
                return int(mod(int(n), m));
            }

            vec4 get_ant_colour(float n) {
                return colours[mod_i(n / ant_size, 2)];
            }

            vec4 checkerboard(vec2 p) {
                return colours[mod_i(p.x, 2) ^ mod_i(p.y, 2)];
            }

            void main() {
                // unflipped uv coordinates
                vec2 uv = vec2(uvCoord.x, 1 - uvCoord.y);
                // uv coordinates in screen space
                vec2 screenUV = uv * screenSize;

                vec4 colour = texture2D(texture, uvCoord) * checkerboard(screenUV / ant_size);
                if (screenUV.x > mouse.x && screenUV.x <= mouse.x + 1) {
                    colour = get_ant_colour(screenUV.y);
                }
                if (screenUV.y > mouse.y && screenUV.y <= mouse.y + 1) {
                    colour = get_ant_colour(screenUV.x);
                }
                outColour = colour;
            }
          '';
      };
    };

    # module.sxhkd.keybindings = {
    #   "Print" = "screenshot-copy";
    #   "shift + Print" = "screenshot-save";
    #   "super + Print" = "screenshot-copy-area";
    # };


    xsession.windowManager.i3.config.keybindings = {
      "Print" = "exec screenshot-copy";
      "shift+Print" = "exec screenshot-save";
      "${config.xsession.windowManager.i3.config.modifier}+Print" = "exec screenshot-copy-area";
    };
  };
}
