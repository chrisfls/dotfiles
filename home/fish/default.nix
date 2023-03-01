{ config, pkgs, lib, specialArgs, ... }:
with lib;
with specialArgs;
let
  cfg = config.module.fish;
in
{
  imports = [
    ../bash
  ];

  options.module.fish = {
    enable = mkEnableOption "fish module";
    bin = mkOption { type = types.nullOr types.str; };
    wsl = {
      enable = mkEnableOption "fish wsl module";
      desktop = mkOption { type = types.nullOr types.str; };
      windir = mkOption { type = types.nullOr types.str; };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      module.bash.enable = true;

      home.packages = with pkgs; [
        fish
        fishPlugins.autopair-fish # probably not needed
        fishPlugins.colored-man-pages
        fishPlugins.done # probably never used
        fishPlugins.foreign-env # probably not needed
        fishPlugins.sponge
        fishPlugins.tide
      ];

      home.file =
        {
          ".profile".text = ''
            if [[ $(ps --no-header --pid=$PPID --format=comm) != "fish" && -z "$BASH_EXECUTION_STRING" ]]
            then
              exec ${cfg.bin}
            fi
          '';
        };

      programs.fish = {
        enable = true;
        shellAliases = {
          "ns" = "nix search nixpkgs";
        };
        shellInit =
          let
            wsl =
              if cfg.wsl.enable then
                "set -g w \"${cfg.wsl.desktop}/\""
              else
                "";
          in
          ''
            set -g SHELL "${cfg.bin}"
            ${(builtins.readFile ./shell_init.fish)}
          '';
        functions = {
          shell = ''
            # prefer using direnv as using nix-shell directly is slow
            # also depends on any-nix-shell to maintain current shell
            nix-shell shell.nix $argv[1..-1]
          '';
          develop = ''
            nix develop "/etc/nixos/shells/$argv[1]" -c fish $argv[2..-1];
          '';
        };
      };
    })

    (mkIf (cfg.enable && cfg.wsl.enable) {
      programs.fish = {
        shellAliases = {
          "neovide" = " /mnt/c/Users/kress/scoop/shims/neovide.exe --wsl";
        };
        functions = {
          code = ''
            if count $argv >/dev/null
              set here (realpath -m $argv[1])
            else
              set here $PWD
            end
            if test -d $here
              powershell "code --folder-uri=vscode-remote://ssh-remote+localhost$here"
            else
              powershell "code --file-uri=vscode-remote://ssh-remote+localhost$here"
            end
          '';
          explorer = ''
            if count $argv >/dev/null
              set here (realpath -m $argv[1])
            else
              set here $PWD
            end
            # TODO: check if $here is a drive letter
            set here (string replace -a '/' '\\' "//${nixosConfig.networking.hostName}.localhost/${distro}$here")
            ${cfg.wsl.windir}/explorer.exe $here
          '';
        };
      };
    })
  ];
}
