{ pkgs, lib, ... }:
predicate: { pkg, ... }@args:
let
  inherit (lib) getExe;
  inherit (lib.attrsets) attrByPath;

  exe =
    attrByPath [ "exe" ] (baseNameOf (getExe pkg)) args;

  lib32 =
    attrByPath [ "lib32" ] false args;

  libp =
    if lib32 then
      "lib32"
    else
      "lib";

  icd =
    if lib32 then
      "share/vulkan/icd.d/intel_icd.i686.json"
    else
      "share/vulkan/icd.d/intel_icd.x86_64.json";
in
if predicate then
  pkgs.symlinkJoin
  {
    name = exe;
    paths = [ pkg ];
    buildInputs = [ pkgs.makeWrapper ];
    meta.mainProgram = exe;
    postBuild = ''
      wrapProgram "$out/bin/${exe}" \
        --set LIBGL_DRIVERS_PATH "/usr/${libp}/dri" \
        --set LIBVA_DRIVERS_PATH "/usr/${libp}/dri" \
        --set __EGL_VENDOR_LIBRARY_FILENAMES "/usr/share/glvnd/egl_vendor.d/50_mesa.json" \
        --set LD_LIBRARY_PATH "/usr/${libp}:/usr/${libp}/vdpau:$LD_LIBRARY_PATH" \
        --set VK_ICD_FILENAMES "/usr/${icd}"

      for desktopFile in $out/share/applications/*; do
        cp --remove-destination $(readlink -e "$desktopFile") "$desktopFile"
        sed -i -e 's:${pkg}/bin/${exe}:${exe}:' "$desktopFile"
      done
    '';
  }
else
  pkg
