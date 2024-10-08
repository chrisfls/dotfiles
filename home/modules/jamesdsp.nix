{ config, lib, pkgs, ... }:
let
  inherit (config.modules.jamesdsp) enable presets;

  crossfeedPresets = {
    weak =
      ''
        crossfeed_mode=0
      '';
    strong =
      ''
        crossfeed_mode=1
      '';
    ooh =
      ''
        crossfeed_mode=2
      '';
    virtualSpeaker30deg3m =
      ''
        crossfeed_mode=99
        crossfeed_bs2b_fcut=700
        crossfeed_bs2b_feed=45
      '';
    chuMoy =
      ''
        crossfeed_mode=99
        crossfeed_bs2b_fcut=700
        crossfeed_bs2b_feed=60
      '';
    janMeier =
      ''
        crossfeed_mode=99
        crossfeed_bs2b_fcut=650
        crossfeed_bs2b_feed=95
      '';
  };

  cf = crossfeedPresets.chuMoy;

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
    '';

  buildCF = target:
    ''
      convolver_enable=false
      crossfeed_enable=true
      ${cf}

      ${buildBase target}
      master_postgain=3
    '';

  buildIR = target:
    ''
      crossfeed_enable=false
      convolver_enable=true
      convolver_file=${./../../assets/audio/convolver/chris130.wav}

      ${buildBase target}
      master_postgain=4.5
    '';

  buildEQ = target:
    ''
      convolver_enable=false
      crossfeed_enable=false

      ${buildBase target}
      master_postgain=3
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
    # REVIEW: move to flatpak
    pacman.packages = [ "chaotic-aur/jamesdsp-git" ];

    xdg.configFile = lib.lists.foldl
      (acc: name: acc // {
        # NOTE: to use df-tilt use `"${name} DF";` instead
        "jamesdsp/presets/${name} (EQ).conf".text = buildEQ name;
        "jamesdsp/presets/${name} (CF).conf".text = buildCF "${name} DF";
        "jamesdsp/presets/${name} (IR).conf".text = buildIR name;
      })
      {
        "jamesdsp/presets/CF.conf".text =
          ''
            graphiceq_enable=false
            convolver_enable=false
            crossfeed_enable=true
            ${cf}

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
