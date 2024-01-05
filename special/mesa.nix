{ pkgs, lib, ... }@args:
let
  inherit (lib) getExe;
  inherit (lib.attrsets) attrByPath;
in
rec {
  wrap = { package, ... }@args:
    let
      exe =
        attrByPath [ "exe" ] (baseNameOf (getExe package)) args;

      lib32 =
        attrByPath [ "lib32" ] false args;

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
    in
    pkgs.symlinkJoin {
      name = exe;
      paths = [ package ];
      buildInputs = [ pkgs.makeWrapper ];
      meta.mainProgram = exe;
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

  wrapIf = predicate: args: if predicate then wrap args else args.package;
}
