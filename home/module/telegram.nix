{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.module.telegram) enable;
  inherit (specialArgs) qt mkIfElse;
  mesa = pkgs.usr.wrapMesaIf config.preset.non-nixos;
  exe = "telegram-desktop";
in
{
  options.module.telegram.enable = lib.mkEnableOption "Enable telegram module";

  config = lib.mkIf enable {
    home.packages = [
      (mesa {
        inherit exe;
        pkg = qt.fixScaling {
          inherit exe;
          pkg = pkgs.telegram-desktop;
        };
      })
    ];

    xsession.windowManager.i3.config = lib.mkIf config.module.i3wm.enable {
      modes.apps."t" = "exec gtk-launch org.telegram; mode default";
      startup = [
        { notification = false; command = "telegram-desktop -startintray"; }
      ];
    };
  };
}
