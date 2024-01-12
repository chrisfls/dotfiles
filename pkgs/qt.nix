{ makeWrapper, symlinkJoin, ... }: {
  fixScaling = pkg:
    symlinkJoin {
      name = pkg.name;
      paths = [ pkg ];
      buildInputs = [ makeWrapper ];
      postBuild =
        ''
          for file in $out/share/applications/*; do
            cp --remove-destination $(readlink -e "$file") "$file"
          done

          for exe in $out/bin/*; do
            wrapProgram "$exe" --set QT_SCREEN_SCALE_FACTORS "" --set QT_AUTO_SCREEN_SCALE_FACTOR 0 --set QT_SCALE_FACTOR "1"
            for file in $out/share/applications/*; do
              bin=$(basename "$exe")
              sed -i -e 's:${pkg}/bin/$bin:$bin:' "$file"
            done
          done
        '';
    };
}
