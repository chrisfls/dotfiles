{ config, lib, ... }:
let
  inherit (config.modules.fontconfig)
    antialias
    autohint
    enable
    hinting
    hintstyle
    lcdfilter
    rgba
    weight;

  toInt = bool: if bool then 1 else 0;

  fromBool = bool: if bool then "true" else "false";
in
{
  options.modules.fontconfig = {
    enable = lib.mkEnableOption "Enable fontconfig module";

    antialias = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };

    hinting = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };

    hintstyle = lib.mkOption {
      # TODO: enum
      type = lib.types.str;
      default = "hintslight";
    };

    rgba = lib.mkOption {
      # TODO: enum
      type = lib.types.str;
      default = "rgb";
    };

    autohint = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };

    lcdfilter = lib.mkOption {
      # TODO: enum
      type = lib.types.str;
      default = "lcddefault";
    };

    weight = lib.mkOption {
      # TODO: enum
      type = lib.types.str;
      default = "medium";
    };
  };

  config = lib.mkIf enable {
    fonts.fontconfig.enable = true;

    xresources.properties = {
      "Xft.antialias" = toInt antialias;
      "Xft.autohint" = toInt autohint;
      "Xft.hinting" = toInt hinting;
      "Xft.hintstyle" = hintstyle;
      "Xft.lcdfilter" = lcdfilter;
      "Xft.rgba" = rgba;
    };

    xdg.configFile."fontconfig/fonts.conf".text = ''
      <?xml version="1.0" encoding="UTF-8"?>
      <!DOCTYPE fontconfig SYSTEM "fonts.dtd">
      <fontconfig>
          <match target="font">
              <edit name="antialias" mode="assign">
                  <bool>${fromBool antialias}</bool>
              </edit>
              <edit name="hinting" mode="assign">
                  <bool>${fromBool hinting}</bool>
              </edit>
              <edit name="hintstyle" mode="assign">
                  <const>${hintstyle}</const>
              </edit>
              <edit name="rgba" mode="assign">
                  <const>${rgba}</const>
              </edit>
              <edit name="autohint" mode="assign">
                  <bool>${fromBool autohint}</bool>
              </edit>
              <edit name="lcdfilter" mode="assign">
                  <const>${lcdfilter}</const>
              </edit>
              <edit name="dpi" mode="assign">
                  <double>${toString config.modules.scaling.dpi}</double>
              </edit>
          </match>
          <match target="font">
              <test name="weight" compare="more">
                  <const>${weight}</const>
              </test>
              <edit name="autohint" mode="assign">
                  <bool>${fromBool autohint}</bool>
              </edit>
          </match>
      </fontconfig>
    '';
  };
}
