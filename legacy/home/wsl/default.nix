{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.module.wsl;
in
{
  options.module.wsl = {
    enable = mkEnableOption "wsl module";
    vscode.enable = mkEnableOption "wsl vscode module";
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home.packages = with pkgs; [
        wslu
        wsl-open
      ];
      home.file = {
        ".local/bin/cmd".source = ./cmd;
        ".local/bin/powershell".source = ./powershell;
      };
    })
    (mkIf (cfg.enable && cfg.vscode.enable)
      (
        let
          nix-ld-so = pkgs.runCommand "ld.so" { } ''
            ln -s "$(cat '${pkgs.stdenv.cc}/nix-support/dynamic-linker')" $out
          '';

          ldEnv = {
            NIX_LD = toString nix-ld-so;
            NIX_LD_LIBRARY_PATH = "/run/current-system/sw/share/nix-ld/lib";
          };

          ldExports = lib.mapAttrsToList (name: value: "export ${name}=${value}") ldEnv;

          joinedLdExports = builtins.concatStringsSep "\n" ldExports;
        in
        {
          home.file = {
            ".vscode-server/server-env-setup".text =
              ''
                ${joinedLdExports}
                sed -i "s/a.execChildProcess(\"uname -m\")/\"$(uname -m)\"/g" $HOME/.vscode-server/extensions/ms-dotnettools.csharp-*-linux-x64/dist/extension.js
              '';
          };
        }
      ))
  ];
}
