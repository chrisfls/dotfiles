{ config, lib, pkgs, ... }:
let
  cfg = config.extra.screenshot;

  screenshot-save = pkgs.writeShellScriptBin "screenshot-save"
    ''
      d="$XDG_PICTURES_DIR/Screenshots/$(date +"%Y-%m-%d")"
      mkdir -p "$d"
      shotgun "$d/$(date +"%H_%M_%S.png")"
    '';

  scale = toString (builtins.ceil config.extra.scaling.scale);

  slop = "slop -r guides -b ${scale}";

  screenshot-area = pkgs.writeShellScriptBin "screenshot-area"
    ''
      shotgun -g \"$(${slop})\" - | xclip -t 'image/png' -selection clipboard
    '';
in
{
  options.extra.screenshot.enable = lib.mkEnableOption "Enable screenshot module";

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.shotgun # or pkgs.maim
      pkgs.slop # or pkgs.hacksaw
      pkgs.xclip
    ];

    extra.nixGL.overlay = {
      shotgun = [ "shotgun" ];
      slop = [ "slop" ];
    };

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

    services.sxhkd.keybindings = {
      "Print" = "shotgun - | xclip -t 'image/png' -selection clipboard";

      "shift + Print" =
        "${screenshot-save}/bin/screenshot-save";

      "super + shift + s" =
        "${screenshot-area}/bin/screenshot-area";
    };
  };
}
