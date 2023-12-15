{ config, lib, pkgs, ... }:
let
  cfg = config.extra;

  recurse = nixgl: final: prev: path: targets:
    let
      pkg =
        lib.attrsets.getAttrFromPath path prev;

      write = bin:
        pkgs.writeShellScriptBin bin "#!/bin/sh\n${nixgl} ${pkg}/bin/${bin}\n";

      targets' =
        if targets == [ ] then
          [ (builtins.baseNameOf (lib.getExe pkg)) ]
        else
          targets;
    in
    (pkgs.symlinkJoin {
      name = "${lib.strings.concatStringsSep "." path}-${builtins.baseNameOf nixgl}";
      paths = (builtins.map write targets') ++ [ pkg ];
    });

  overlay = { package, overlay, ... }: final: prev:
    lib.attrsets.recursiveUpdate prev
      (lib.attrsets.mapAttrsRecursive
        (recurse (lib.getExe package) final prev)
        overlay);
in
{
  options.extra = {
    nixGL = {
      enable = lib.mkEnableOption "Enable NixGL wrapper for opengl";
      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.nixgl.nixGLIntel;
      };
      overlay = lib.mkOption {
        type = lib.types.attrs;
        default = { };
      };
    };

    nixVulkan = {
      enable = lib.mkEnableOption "Enable NixGL wrapper for vulkan";
      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.nixgl.nixVulkanIntel;
      };
      overlay = lib.mkOption {
        type = lib.types.attrs;
        default = { };
      };
    };
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.nixGL.enable {
      home.packages = [ cfg.nixGL.package ];
      nixpkgs.overlays = [ (overlay cfg.nixGL) ];
    })

    (lib.mkIf cfg.nixVulkan.enable {
      home.packages = [ cfg.nixVulkan.package ];
      nixpkgs.overlays = [ (overlay cfg.nixVulkan) ];
    })
  ];
}
