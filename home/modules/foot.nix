{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.modules.foot) enable;

  color-scheme = lib.attrsets.mapAttrs
    (name: builtins.replaceStrings [ "#" ] [ "" ])
    config.modules.foot.color-scheme;
in
{
  options.modules.foot = {
    enable = lib.mkEnableOption "Enable foot module";

    color-scheme = lib.mkOption {
      type = lib.types.attrs;
      default = specialArgs.color-schemes.breeze;
    };
  };

  config = lib.mkIf enable {
    pacman.packages = [ "extra/foot" ];

    xdg.configFile."foot/foot.ini".text =
      with color-scheme;
      ''
        [main]
        shell= fish
        dpi-aware= no
        initial-window-size-chars= 80x24
        font=CaskaydiaCove NFM:size=10

        [scrollback]
        lines= 1000000

        [cursor]
        style= beam
        blink= yes
        color= ${backgroundBright} ${foregroundBright}

        [colors]
        background= ${background}
        foreground= ${foreground}

        selection-foreground= ${backgroundDim}
        selection-background= ${foregroundDim}

        flash= ${foregroundBright}

        regular0= ${black}
        regular1= ${red}
        regular2= ${green}
        regular3= ${yellow}
        regular4= ${blue}
        regular5= ${magenta}
        regular6= ${cyan}
        regular7= ${white}

        bright0= ${blackBright}
        bright1= ${redBright}
        bright2= ${greenBright}
        bright3= ${yellowBright}
        bright4= ${blueBright}
        bright5= ${magentaBright}
        bright6= ${cyanBright}
        bright7= ${whiteBright}

        dim0= ${blackDim}
        dim1= ${redDim}
        dim2= ${greenDim}
        dim3= ${yellowDim}
        dim4= ${blueDim}
        dim5= ${magentaDim}
        dim6= ${cyanDim}
        dim7= ${whiteDim}
      '';
  };
}
