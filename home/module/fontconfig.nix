{ config, lib, ... }:
let
  cfg = config.extra.fontconfig;

  dpi = toString config.extra.scaling.dpi;

  toInt = bool: if bool then 1 else 0;
in
{
  options.extra.fontconfig = {
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

  config = lib.mkIf cfg.enable {
    fonts.fontconfig.enable = true;

    xresources.properties = {
      "Xft.antialias" = toInt cfg.antialias;
      "Xft.autohint" = toInt cfg.autohint;
      "Xft.hinting" = toInt cfg.hinting;
      "Xft.hintstyle" = cfg.hintstyle;
      "Xft.lcdfilter" = cfg.lcdfilter;
      "Xft.rgba" = cfg.rgba;
    };

    xdg.configFile."fontconfig/fonts.conf".text = ''
      <?xml version="1.0" encoding="UTF-8"?>
      <!DOCTYPE fontconfig SYSTEM "fonts.dtd">
      <fontconfig>
          <match target="font">
              <edit name="antialias" mode="assign">
                  <bool>${toString cfg.antialias}</bool>
              </edit>
              <edit name="hinting" mode="assign">
                  <bool>${toString cfg.hinting}</bool>
              </edit>
              <edit name="hintstyle" mode="assign">
                  <const>${cfg.hintstyle}</const>
              </edit>
              <edit name="rgba" mode="assign">
                  <const>${cfg.rgba}</const>
              </edit>
              <edit name="autohint" mode="assign">
                  <bool>${toString cfg.autohint}</bool>
              </edit>
              <edit name="lcdfilter" mode="assign">
                  <const>${cfg.lcdfilter}</const>
              </edit>
              <edit name="dpi" mode="assign">
                  <double>${dpi}</double>
              </edit>
          </match>
          <match target="font">
              <test name="weight" compare="more">
                  <const>${cfg.weight}</const>
              </test>
              <edit name="autohint" mode="assign">
                  <bool>${toString cfg.autohint}</bool>
              </edit>
          </match>
      </fontconfig>
    '';
  };
}
