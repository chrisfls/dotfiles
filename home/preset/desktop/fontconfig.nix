{ config, lib, pkgs, specialArgs, ... }:
let
  cfg = config.extra;
  dpi = cfg.dpi;
in
{
  # TODO: abstract this away
  config = {
    xresources.properties = {
      "Xft.dpi" = dpi;
      "Xft.antialias" = 1;
      "Xft.autohint" = 0;
      "Xft.hinting" = 1;
      "Xft.hintstyle" = "hintslight";
      "Xft.lcdfilter" = "lcddefault";
      "Xft.rgba" = "rgb";
    };

    xdg.configFile."fontconfig/fonts.conf".text = ''
      <?xml version="1.0" encoding="UTF-8"?>
      <!DOCTYPE fontconfig SYSTEM "fonts.dtd">
      <fontconfig>
          <match target="font">
              <edit name="antialias" mode="assign">
                  <bool>true</bool>
              </edit>
              <edit name="hinting" mode="assign">
                  <bool>true</bool>
              </edit>
              <edit name="hintstyle" mode="assign">
                  <const>hintslight</const>
              </edit>
              <edit name="rgba" mode="assign">
                  <const>rgb</const>
              </edit>
              <edit name="autohint" mode="assign">
                  <bool>false</bool>
              </edit>
              <edit name="lcdfilter" mode="assign">
                  <const>lcddefault</const>
              </edit>
              <edit name="dpi" mode="assign">
                  <double>${builtins.toString dpi}</double>
              </edit>
          </match>
          <match target="font">
              <test name="weight" compare="more">
                  <const>medium</const>
              </test>
              <edit name="autohint" mode="assign">
                  <bool>false</bool>
              </edit>
          </match>
      </fontconfig>
    '';

    home.sessionVariables = {
      QT_FONT_DPI = dpi;
    };
  };
}
