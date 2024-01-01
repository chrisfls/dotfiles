{ pkgs, lib, ... }:
let
  inherit (lib) getExe;
  inherit (lib.attrsets) attrByPath;
in
{
  fixScaling = { package, ... }@args:
    let
      bin = attrByPath [ "bin" ] (baseNameOf (getExe package)) args;
    in
    pkgs.symlinkJoin {
      name = bin;
      paths = [ package ];
      buildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        wrapProgram "$out/bin/${bin}" --set QT_SCREEN_SCALE_FACTORS "" --set QT_AUTO_SCREEN_SCALE_FACTOR 0 --set QT_SCALE_FACTOR "1"
        for desktopFile in $out/share/applications/*; do
          cp --remove-destination $(readlink -e "$desktopFile") "$desktopFile"
          sed -i -e 's:${package}/bin/${bin}:${bin}:' "$desktopFile"
        done
      '';
    };
}
