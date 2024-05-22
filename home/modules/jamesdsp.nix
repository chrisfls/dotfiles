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
 
  buildBase = name:
    let 
      name' = "${name} + L.txt";
      graphicEQ = lib.readFile ../../assets/audio/equalizer/${name'};
    in
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
      convolver_file=${./../../assets/audio/convolver/chris130eq.wav}

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
      type = lib.types.listOf lib.types.str;
      default = [
        "7Hz-Salnotes Zero"
        "CCA CRA"
        "Etymotic ER2SE"
        "Moondrop Aria SE"
        "Moondrop Chu"
        "Philips SHP9500"
      ];
    };
  };

  config = lib.mkIf enable {
    pacman.packages = [ "chaotic-aur/jamesdsp-git" ];

    xdg.configFile = lib.lists.foldl
      (acc: name: acc // {
        "jamesdsp/presets/${name} (EQ).conf".text = buildEQ name;
        "jamesdsp/presets/${name} (CF).conf".text = buildCF name;
        "jamesdsp/presets/${name} (IR).conf".text = buildIR "${name} DF";
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
            convolver_file=${./../../assets/audio/convolver/chris130eq.wav}

            ${base}
            master_postgain=0
          '';
      }
      presets;
  };
}
