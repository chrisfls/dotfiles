{ config, lib, pkgs, ... }:
let
  inherit (config.pacman) enable packages explicits;

  quote = str: "'${str}'";

  repo-packages = lib.trivial.pipe packages [
    (builtins.filter (name: !(lib.strings.hasPrefix "aur/" name)))
    (builtins.map quote)
    (builtins.concatStringsSep " ")
  ];

  aur-packages = lib.trivial.pipe packages [
    (builtins.filter (name: lib.strings.hasPrefix "aur/" name))
    (builtins.map quote)
    (builtins.concatStringsSep " ")
  ];

  all-packages = lib.trivial.pipe (packages ++ explicits) [
    (builtins.map
      (builtins.replaceStrings
        [ "aur/" "chaotic-aur/" "core/" "extra/" "multilib/" ]
        [ "" "" "" "" "" ]))
    (builtins.map quote)
    (builtins.concatStringsSep " ")
  ];
in
{
  options.pacman = {
    enable = lib.mkEnableOption "Enable pacman module";

    packages = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
    };

    explicits = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
    };
  };

  config = lib.mkIf enable {
    pacman.packages = [
      # system
      "core/base"
      "core/base-devel"
      "extra/linux-zen"
      # chaotic
      "chaotic-aur/chaotic-keyring"
      "chaotic-aur/chaotic-mirrorlist"
      "chaotic-aur/paru"
      # builtins
      "extra/pacman-contrib"
      "extra/nix"
    ];

    home.packages = [
      (pkgs.writeHostScriptBin "pacman-switch"
        ''
          # mark all packages as deps
          pacman -Qq | sudo pacman -D --asdeps -

          # mark these packages as explicts
          sudo pacman -D --asexplicit  ${all-packages}

          # update everything
          sudo pacman -Sy --needed --noconfirm archlinux-keyring chaotic-keyring && \
          sudo pacman -Su --needed --noconfirm ${repo-packages} && \
          paru -Su --needed --noconfirm ${aur-packages}

          # remove unneeded packages
          pacman -Qdtq | sudo pacman -Rs -

          desktop-switch
        '')

      (pkgs.writeHostScriptBin "desktop-switch"
        ''
          source_dir="/usr/share/applications/"
          target_dir="$XDG_DATA_HOME/applications/"

          mkdir -p "$target_dir"

          for desktop_file in "$source_dir"*.desktop; do
            filename=$(basename "$desktop_file")
            original_content=$(cat "$desktop_file")
            modified_content=$(echo "$original_content" | sed 's|/usr/bin/||g')

            if [[ "$modified_content" != "$original_content" ]]; then
              cp -f "$desktop_file" "$target_dir$filename"
              echo "Copied $filename"
            fi
          done
        '')
    ];
  };
}
