{ pkgs, lib, ... }@args:
let
  inherit (lib) getExe;
  inherit (lib.attrsets) hasAttr;

  mkIfElse = import ./mk-if-else.nix args;
in
{
  wrapIf = predicate: { package, ... }@args:
    let
      exe =
        if hasAttr [ "exe" ] args then
          args.exe
        else
          baseNameOf (getExe package);

      lib32 =
        if hasAttr [ "lib32" ] args then
          args.lib32
        else
          false;

      lib =
        if lib32 then
          "/usr/lib32"
        else
          "/usr/lib";

      icd =
        if lib32 then
          "/usr/share/vulkan/icd.d/intel_icd.i686.json"
        else
          "/usr/share/vulkan/icd.d/intel_icd.x86_64.json";

      package' = pkgs.symlinkJoin {
        name = exe;
        paths = [ package ];
        buildInputs = [ pkgs.makeWrapper ];
        postBuild = ''
          wrapProgram "$out/bin/${exe}" \
            --set LIBGL_DRIVERS_PATH "${lib}/dri" \
            --set LIBVA_DRIVERS_PATH "${lib}/dri" \
            --set __EGL_VENDOR_LIBRARY_FILENAMES "/usr/share/glvnd/egl_vendor.d/50_mesa.json" \
            --set LD_LIBRARY_PATH "${lib}:${lib}/vdpau:$LD_LIBRARY_PATH" \
            --set VK_ICD_FILENAMES "${icd}"

          for desktopFile in $out/share/applications/*; do
            cp --remove-destination $(readlink -e "$desktopFile") "$desktopFile"
            sed -i -e 's:${package}/bin/${exe}:${exe}:' "$desktopFile"
          done
        '';
      };
    in
    mkIfElse predicate package' package;
}
