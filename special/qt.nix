{ symlinkJoin, makeWrapper, ... }:
{
  fixScaling = pkg: bin:
    symlinkJoin {
      name = bin;
      paths = [ pkg ];
      buildInputs = [ makeWrapper ];
      postBuild = ''
        wrapProgram "$out/bin/${bin}" --set QT_SCREEN_SCALE_FACTORS 0 --set QT_AUTO_SCREEN_SCALE_FACTOR 0
        for desktopFile in $out/share/applications/*; do
          cp --remove-destination $(readlink -e "$desktopFile") "$desktopFile"
          sed -i -e 's:${pkg}/bin/${bin}:${bin}:' "$desktopFile"
        done
      '';
    };
}
