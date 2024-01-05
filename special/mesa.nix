{ pkgs, lib, ... }:
let
  inherit (lib) getExe;
  inherit (lib.attrsets) attrByPath;

  base = lib: icd: { package, ... }@args:
    let exe = attrByPath [ "exe" ] (baseNameOf (getExe package)) args;
    in pkgs.symlinkJoin {
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

  wrap = base "/usr/lib" "/usr/share/vulkan/icd.d/intel_icd.x86_64.json";

  wrap32 = base "/usr/lib32" "/usr/share/vulkan/icd.d/intel_icd.i686.json";
in
{
  wrapIf = predicate: { package, ... }@args:
    let lib32 = attrByPath [ "lib32" ] false args;
    in lib.mkMerge [
      (lib.mkIf predicate (if lib32 then wrap32 args else wrap args))
      (lib.mkIf (!predicate) package)
    ];
}
