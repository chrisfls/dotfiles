{ config, lib, pkgs, ... }:
let
  inherit (config.modules.jamesdsp) enable presets;

  base =
    ''
      bass_enable=false
      compander_enable=false
      liveprog_enable=false
      reverb_enable=false
      stereowide_enable=false
      tone_enable=false
      tube_enable=false

      master_enable=true
      master_limrelease=100
      master_limthreshold=0
    '';

  buildBase = path:
    let graphicEQ = lib.readFile path; in
    ''
      graphiceq_enable=true
      graphiceq_param=${graphicEQ}

      ${base}
      master_postgain=6
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
      convolver_file=${./../../assets/audio/convolver/chris130.wav}

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
  options.modules.jamesdsp = {
    enable = lib.mkEnableOption "Enable jamesdsp module";

    presets = lib.mkOption {
      type = lib.types.attrsOf lib.types.path;
      default = {
        "7Hz-Salnotes Zero" = ../../assets/audio/equalizer/${"7Hz-Salnotes Zero + L.txt"};
        "CCA CRA" = ../../assets/audio/equalizer/${"CCA CRA + L.txt"};
        "Etymotic ER2SE" = ../../assets/audio/equalizer/${"Etymotic ER2SE + L.txt"};
        "Moondrop Aria SE" = ../../assets/audio/equalizer/${"Moondrop Aria SE + L.txt"};
        "Moondrop Chu" = ../../assets/audio/equalizer/${"Moondrop Chu + L.txt"};
        # this version might trigger the limiter too often
        "Philips SHP9500" = ../../assets/audio/equalizer/${"Philips SHP9500 + L.txt"};
      };
    };
  };

  config = lib.mkIf enable {
    pacman.packages = [ "chaotic-aur/jamesdsp-git" ];

    xdg.configFile = lib.attrsets.foldlAttrs
      (acc: name: path: acc // {
        "jamesdsp/presets/${name} (EQ).conf".text = buildEQ path;
        "jamesdsp/presets/${name} (CF).conf".text = buildCF path;
        "jamesdsp/presets/${name} (IR).conf".text = buildIR path;
      })
      {
        "jamesdsp/presets/CF.conf".text =
          ''
            graphiceq_enable=false
            convolver_enable=false
            crossfeed_enable=true
            crossfeed_mode=0
            crossfeed_bs2b_fcut=700
            crossfeed_bs2b_feed=60

            ${base}
            master_postgain=0
          '';

        "jamesdsp/presets/IR.conf".text =
          ''
            graphiceq_enable=false
            crossfeed_enable=false
            convolver_enable=true
            convolver_file=${./../../assets/audio/convolver/chris130.wav}

            ${base}
            master_postgain=0
          '';
      }
      presets;
  };
}
