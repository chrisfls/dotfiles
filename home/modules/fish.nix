{ config, lib, pkgs, ... }:
let inherit (config.modules.fish) enable extraConfig;
  aliases = {
    "g" = "git";
  };

  plugins = [
    "ilancosman/tide"
    "oh-my-fish/plugin-foreign-env"
  ];
in
{
  options.modules.fish.enable = lib.mkEnableOption "Enable fish module";
  options.modules.fish.extraConfig = lib.mkOption { type = lib.types.lines; default = ""; };

  config = lib.mkIf enable {
    pacman.packages = [
      "extra/fish"
      "extra/fisher"
    ];

    xdg.configFile."fish/config.fish".text =
      let
        aliases' = builtins.concatStringsSep "\n" (lib.attrsets.mapAttrsToList (n: v: "alias ${n} ${v}") aliases);
        plugins' = builtins.concatStringsSep " " plugins;
      in
      ''
        # only execute this file once per shell.
        set -q startup; and exit
        set -g startup 1

        set fish_greeting ""

        status --is-interactive; and begin
          if not set -q first_setup
            echo "fisher install"
            fisher install ${plugins'}

            echo "tide setupy"
            echo 1 2 1 2 3 2 1 1 y | tide configure >/dev/null
            set --universal tide_character_icon λ

            set --universal first_setup true
          end
        end

        fenv source $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh

        set -g SHELL (which fish)

        status --is-interactive; and begin
          # aliases
          ${aliases'}

          # add completions generated by Home Manager to $fish_complete_path
          begin
              set -l joined (string join " " $fish_complete_path)
              set -l prev_joined (string replace --regex "[^\s]*generated_completions.*" "" $joined)
              set -l post_joined (string replace $prev_joined "" $joined)
              set -l prev (string split " " (string trim $prev_joined))
              set -l post (string split " " (string trim $post_joined))
              set fish_complete_path $prev "$XDG_DATA_HOME/fish/home-manager_generated_completions" $post
          end

          ${extraConfig}
        end
      '';
  };
}
