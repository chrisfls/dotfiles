rec {
  # ported from 
  # https://github.com/atomcorp/themes/blob/master/themes.json
  popping-and-locking = {
    background = "#181921";
    black = "#1D2021";
    blackBright = "#928374";
    blue = "#458588";
    blueBright = "#99C6CA";
    cursorColor = "#C7C7C7";
    cyan = "#689D6A";
    cyanBright = "#7EC16E";
    foreground = "#EBDBB2";
    green = "#98971A";
    greenBright = "#B8BB26";
    magenta = "#B16286";
    magentaBright = "#D3869B";
    red = "#CC241D";
    redBright = "#F42C3E";
    selectionBackground = popping-and-locking.foreground;
    white = "#A89984";
    whiteBright = popping-and-locking.foreground;
    yellow = "#D79921";
    yellowBright = "#FABD2F";
  };
  # adapted from popping-and-locking and colors from popping-and-locking-black vscode theme
  # https://github.com/Luxcium/pop-n-lock-theme-vscode
  popping-and-locking-black = {
    inherit (popping-and-locking)
      black
      blue
      cyan
      green
      magenta
      red
      white
      yellow
      blackBright
      blueBright
      cyanBright
      greenBright
      magentaBright
      redBright
      whiteBright
      yellowBright;
    background = "#000000";
    foreground = "#F2E5BC";
    cursorColor = popping-and-locking-black.foreground;
    selectionBackground = "#6A6B5D";
  };
  # ported from
  # https://github.com/PapirusDevelopmentTeam/arc-kde/blob/master/konsole/ArcDark.colorscheme
  arc-dark = {
    background = "#383c4a";
    foreground = "#d3dae3";

    cursorColor = arc-dark.foreground;
    selectionBackground = arc-dark.background;

    black = "#4b5164";
    red = "#e14245";
    green = "#5ca75b";
    yellow = "#f6ab32";
    blue = "#4877b1";
    magenta = "#a660c3";
    cyan = "#5294e2";
    white = "#a9a9aa";

    blackBright = "#616981";
    redBright = "#e16f7e";
    greenBright = "#add488";
    yellowBright = "#fdc35f";
    blueBright = "#8ca9bf";
    magentaBright = "#e2afec";
    cyanBright = "#73c5e2";
    whiteBright = "#fcfcfc";

    blackDim = "#2c2f3a";
    redDim = "#783228";
    greenDim = "#4b7c16";
    yellowDim = "#b65619";
    blueDim = "#2b486b";
    magentaDim = "#614a73";
    cyanDim = "#1b668f";
    whiteDim = "#63686d";
  };
  # ported from
  # https://gist.github.com/Kanon/99840108309ee3d2b995a0e5714f73ad
  arc-dark-alt = {
    background = "#2f343f";
    foreground = "#f3f4f5";

    cursorColor = arc-dark.foreground;
    selectionBackground = arc-dark.background;

    black = "#262b36";
    red = "#9c3528";
    green = "#61bc3b";
    yellow = "#f3b43a";
    blue = "#0d68a8";
    magenta = "#744560";
    cyan = "#288e9c";
    white = "#a2a2a2";

    blackBright = "#2f343f";
    redBright = "#d64937";
    greenBright = "#86df5d";
    yellowBright = "#fdd75a";
    blueBright = "#0f75bd";
    magentaBright = "#9e5e83";
    cyanBright = "#37c3d6";
    whiteBright = "#f9f9f9";
  };
  # ported from
  # https://github.com/alacritty/alacritty-theme/blob/master/themes/material_theme_mod.toml
  material-mod = {
    background = "#1e282d";
    foreground = "#c4c7d1";

    black = "#666666";
    red = "#eb606b";
    green = "#c3e88d";
    yellow = "#f7eb95";
    blue = "#80cbc4";
    magenta = "#ff2f90";
    cyan = "#aeddff";
    white = "#ffffff";

    blackBright = "#a1a1a1";
    redBright = "#eb606b";
    greenBright = "#c3e88d";
    yellowBright = "#f7eb95";
    blueBright = "#7dc6bf";
    magentaBright = "#6c71c4";
    cyanBright = "#35434d";
    whiteBright = "#ffffff";
  };
  # ported from
  # https://github.com/carloscuesta/materialshell/blob/master/shell-color-themes/windows/terminal/dark.json
  material-shell = {
    background = "#151515";
    foreground = "#A1B0B8";

    cursorColor = "#FFFFFF";
    selectionBackground = "#9cb0b9";

    black = "#252525";
    red = "#FF443E";
    green = "#C3D82C";
    yellow = "#FFC135";
    blue = "#42A5F5";
    magenta = "#C594C5";
    cyan = "#00ACC1";
    white = "#F5F5F5";

    blackBright = "#A1B0B8";
    redBright = "#EC5F67";
    greenBright = "#99C794";
    yellowBright = "#FAC863";
    blueBright = "#6699CC";
    magentaBright = "#D81B60";
    cyanBright = "#5FB3B3";
    whiteBright = "#D8DEE9";
  };
  material-dark = {
    background = "#212121";
    foreground = "#eeffff";

    cursorColor = "#ffffff";
    selectionBackground = "#eeffff";

    black = "#000000";
    red = "#ff5370";
    green = "#c3e88d";
    yellow = "#ffcb6b";
    blue = "#82aaff";
    magenta = "#c792ea";
    cyan = "#89ddff";
    white = "#ffffff";

    blackBright = "#545454";
    redBright = "#ff5370";
    greenBright = "#c3e88d";
    yellowBright = "#ffcb6b";
    blueBright = "#82aaff";
    magentaBright = "#c792ea";
    cyanBright = "#89ddff";
    whiteBright = "#ffffff";
  };
  # ported from
  # https://github.com/PapirusDevelopmentTeam/materia-kde/blob/master/konsole/MateriaDark.colorscheme
  material-dark-kde = {
    background = "#121212";
    foreground = "#DFDFDF";

    black = "#474747";
    red = "#F44336";
    green = "#4CAF50";
    yellow = "#FF9800";
    blue = "#1A73E8";
    magenta = "#9C27B0";
    cyan = "#0097A7";
    white = "#FFFFFF";

    backgroundBright = "#1E1E1E";
    foregroundBright = "#FFFFFF";

    blackBright = "#474747";
    redBright = "#F44336";
    greenBright = "#4CAF50";
    yellowBright = "#FF9800";
    blueBright = "#1A73E8";
    magentaBright = "#9C27B0";
    cyanBright = "#0097A7";
    whiteBright = "#FFFFFF";

    backgroundDim = "#121212";
    foregroundDim = "#DFDFDF";

    blackDim = "#474747";
    redDim = "#F44336";
    greenDim = "#4CAF50";
    yellowDim = "#FF9800";
    blueDim = "#1A73E8";
    magentaDim = "#9C27B0";
    cyanDim = "#0097A7";
    whiteDim = "#FFFFFF";
  };
  material-dark-kde-alt = {
    background = "#121212";
    foreground = "#E0E0E0";

    black = "#474747";
    red = "#F44336";
    green = "#4CAF50";
    yellow = "#FF9800";
    blue = "#1A73E8";
    magenta = "#9C27B0";
    cyan = "#0097A7";
    white = "#FAFAFA";

    backgroundDim = "#121212";
    foregroundDim = "#9E9E9E";

    blackDim = "#000000";
    redDim = "#E53935";
    greenDim = "#43A047";
    yellowDim = "#FB8C00";
    blueDim = "#1565C0";
    magentaDim = "#8E24AA";
    cyanDim = "#00838F";
    whiteDim = "#F5F5F5";

    backgroundBright = "#121212";
    foregroundBright = "#F5F5F5";

    blackBright = "#616161";
    redBright = "#EF5350";
    greenBright = "#66BB6A";
    yellowBright = "#FFA726";
    blueBright = "#2196F3";
    magentaBright = "#AB47BC";
    cyanBright = "#00ACC1";
    whiteBright = "#FFFFFF";
  };
}
