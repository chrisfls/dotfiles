{ config, lib, pkgs, ... }:
let
  cfg = config.extra;

  wrap = lib.mkOption {
    type = lib.types.attrs; /*Of (lib.types.submodule {
      options.enable = lib.mkEnableOption "Enable wrapper for this package";
      options.targets = lib.mkOption {
        type = lib.types.nullOr (lib.types.listOf lib.types.str);
        description = "List of executables to wrap.";
        default = null;
      };
    });*/
    default = { };
  };

  expand = nixgl: final: prev: name: entry:
    let
      path = lib.strings.splitString "." name;

      pkg =
        lib.attrsets.getAttrFromPath path prev;

      write = a: pkgs.writeShellScriptBin a "#!/bin/sh\n${nixgl} ${pkg}/bin/${a}\n";

      targets =
        lib.attrsets.attrByPath [ "targets" ]
          [ (lib.getExe pkg) ]
          entry;

    in
    if entry.enable then
      lib.attrsets.updateManyAttrsByPath
        [
          {
            inherit path;
            update = old:
              (pkgs.symlinkJoin {
                inherit name;
                paths =
                  (builtins.map write targets)
                  ++ [ pkg ];
              });
          }
        ]
        prev

    else
      { };

  overlay = { package, wrap, ... }: final: prev:
    lib.attrsets.concatMapAttrs (expand (lib.getExe package) final prev) wrap;
in
{
  options.extra = {
    nixGL = {
      enable = lib.mkEnableOption "Enable NixGL wrapper for opengl";
      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.nixgl.nixGLIntel;
      };
      wrap = wrap;
    };

    nixVulkan = {
      enable = lib.mkEnableOption "Enable NixGL wrapper for vulkan";
      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.nixgl.nixVulkanIntel;
      };
      wrap = wrap;
    };
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.nixGL.enable {
      home.packages = [ cfg.nixGL.package ];
      nixpkgs.overlays = [
        (overlay cfg.nixGL)
      ];
    })

    (lib.mkIf cfg.nixVulkan.enable {
      home.packages = [ cfg.nixVulkan.package ];
    })
  ];
}
