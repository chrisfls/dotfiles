{ config, lib, pkgs, ... }:
let
  inherit (config.module.jamesdsp) enable presets;

  buildBase = path:
    let graphicEQ = lib.readFile path; in
    ''
      graphiceq_enable=true
      graphiceq_param=${graphicEQ}

      master_enable=true
      master_limrelease=61
      master_limthreshold=0
      master_postgain=6

      bass_enable=false
      compander_enable=false
      liveprog_enable=false
      reverb_enable=false
      stereowide_enable=false
      tone_enable=false
      tube_enable=false
    '';

  buildCF = target:
    ''
      convolver_enable=false
      crossfeed_enable=true
      crossfeed_mode=0
      crossfeed_bs2b_fcut=700
      crossfeed_bs2b_feed=60

      ${buildBase target}
    '';

  buildIR = target:
    ''
      crossfeed_enable=false
      convolver_enable=true
      convolver_file=${./../../assets/audio/convolver/kress130.wav}

      ${buildBase target}
    '';

  buildEQ = target:
    ''
      convolver_enable=false
      crossfeed_enable=false

      ${buildBase target}
    '';
in
{
  options.module.jamesdsp = {
    enable = lib.mkEnableOption "Enable jamesdsp module";

    presets = lib.mkOption {
      type = lib.types.attrsOf lib.types.path;
      default = {
        "7Hz-Salnotes Zero" = ../../assets/audio/equalizer/${"7Hz-Salnotes Zero +.txt"};
        "CCA CRA" = ../../assets/audio/equalizer/${"CCA CRA +.txt"};
        "Etymotic ER2SE" = ../../assets/audio/equalizer/${"Etymotic ER2SE +.txt"};
        "Moondrop Aria SE" = ../../assets/audio/equalizer/${"Moondrop Aria SE +.txt"};
        "Moondrop Chu" = ../../assets/audio/equalizer/${"Moondrop Chu +.txt"};
        # this version might trigger the limiter too often
        "Philips SHP9500" = ../../assets/audio/equalizer/${"Philips SHP9500 +.txt"};
      };
    };
  };

  config = lib.mkIf enable {
    home.packages = [ pkgs.jamesdsp ];

    xsession.windowManager.i3.config.startup = lib.mkIf config.module.i3wm.enable [
      { notification = false; command = "${pkgs.jamesdsp}/bin/jamesdsp --tray"; }
    ];

    xdg.configFile = lib.attrsets.foldlAttrs
      (acc: name: path: acc // {
        "jamesdsp/presets/${name} (EQ).conf".text = buildEQ path;
        "jamesdsp/presets/${name} (CF).conf".text = buildCF path;
        "jamesdsp/presets/${name} (IR).conf".text = buildIR path;
      })
      { }
      presets;
  };
}
