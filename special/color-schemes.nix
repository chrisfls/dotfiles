rec {
  # too ugly but follows my gtk/kde theme
  materia-dark-kde = {
    # source: https://github.com/PapirusDevelopmentTeam/materia-kde/blob/master/konsole/MateriaDark.colorscheme
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
  # great contrast with ugly colors
  tomorrow-night-bright = {
    background = "#000000";
    foreground = "#eaeaea";

    black = "#000000";
    red = "#d54e53";
    green = "#b9ca4a";
    yellow = "#e6c547";
    blue = "#7aa6da";
    magenta = "#c397d8";
    cyan = "#70c0ba";
    white = "#424242";

    backgroundDim = "#000000";
    foregroundDim = "#eaeaea";

    blackDim = "#000000";
    redDim = "#d54e53";
    greenDim = "#b9ca4a";
    yellowDim = "#e6c547";
    blueDim = "#7aa6da";
    magentaDim = "#c397d8";
    cyanDim = "#70c0ba";
    whiteDim = "#424242";

    backgroundBright = "#000000";
    foregroundBright = "#eaeaea";

    blackBright = "#666666";
    redBright = "#ff3334";
    greenBright = "#9ec400";
    yellowBright = "#e7c547";
    blueBright = "#7aa6da";
    magentaBright = "#b77ee0";
    cyanBright = "#54ced6";
    whiteBright = "#2a2a2a";
  };
  # reminds me too much of windows D:
  campbell = {
    background = "#0C0C0C";
    foreground = "#CCCCCC";
    black = "#0C0C0C";
    red = "#C50F1F";
    green = "#13A10E";
    yellow = "#C19C00";
    blue = "#0037DA";
    magenta = "#881798";
    cyan = "#3A96DD";
    white = "#CCCCCC";

    backgroundDim = "#0C0C0C";
    foregroundDim = "#CCCCCC";
    blackDim = "#0C0C0C";
    redDim = "#C50F1F";
    greenDim = "#13A10E";
    yellowDim = "#C19C00";
    blueDim = "#0037DA";
    magentaDim = "#881798";
    cyanDim = "#3A96DD";
    whiteDim = "#CCCCCC";

    backgroundBright = "#0C0C0C";
    foregroundBright = "#CCCCCC";
    blackBright = "#767676";
    redBright = "#E74856";
    greenBright = "#16C60C";
    yellowBright = "#F9F1A5";
    blueBright = "#3B78FF";
    magentaBright = "#B4009E";
    cyanBright = "#61D6D6";
    whiteBright = "#F2F2F2";
  };
  # bad contrast with blues
  xterm = {
    background = "#000000";
    foreground = "#ffffff";
    black = "#000000";
    red = "#cd0000";
    green = "#00cd00";
    yellow = "#cdcd00";
    blue = "#0000ee";
    magenta = "#cd00cd";
    cyan = "#00cdcd";
    white = "#e5e5e5";

    backgroundDim = "#000000";
    foregroundDim = "#ffffff";
    blackDim = "#000000";
    redDim = "#cd0000";
    greenDim = "#00cd00";
    yellowDim = "#cdcd00";
    blueDim = "#0000ee";
    magentaDim = "#cd00cd";
    cyanDim = "#00cdcd";
    whiteDim = "#e5e5e5";

    backgroundBright = "#000000";
    foregroundBright = "#ffffff";
    blackBright = "#7f7f7f";
    redBright = "#ff0000";
    greenBright = "#00ff00";
    yellowBright = "#ffff00";
    blueBright = "#5c5cff";
    magentaBright = "#ff00ff";
    cyanBright = "#00ffff";
    whiteBright = "#ffffff";
  };
  # solves issues with xterm, but is too bright
  hyper = {

    # [colors.cursor]
    # text = "#F81CE5";
    # cursor = "#ffffff";

    background = "#000000";
    foreground = "#ffffff";
    black = "#000000";
    red = "#fe0100";
    green = "#33ff00";
    yellow = "#feff00";
    blue = "#0066ff";
    magenta = "#cc00ff";
    cyan = "#00ffff";
    white = "#d0d0d0";

    backgroundDim = "#000000";
    foregroundDim = "#ffffff";
    blackDim = "#000000";
    redDim = "#fe0100";
    greenDim = "#33ff00";
    yellowDim = "#feff00";
    blueDim = "#0066ff";
    magentaDim = "#cc00ff";
    cyanDim = "#00ffff";
    whiteDim = "#d0d0d0";

    backgroundBright = "#000000";
    foregroundBright = "#ffffff";
    blackBright = "#808080";
    redBright = "#fe0100";
    greenBright = "#33ff00";
    yellowBright = "#feff00";
    blueBright = "#0066ff";
    magentaBright = "#cc00ff";
    cyanBright = "#00ffff";
    whiteBright = "#FFFFFF";
  };
  # too dim, ugly blues
  terminal-app = {
    background = "#000000";
    foreground = "#b6b6b6";
    black = "#000000";
    red = "#990000";
    green = "#00a600";
    yellow = "#999900";
    blue = "#0000b2";
    magenta = "#b200b2";
    cyan = "#00a6b2";
    white = "#bfbfbf";

    backgroundDim = "#000000";
    foregroundDim = "#b6b6b6";
    blackDim = "#000000";
    redDim = "#990000";
    greenDim = "#00a600";
    yellowDim = "#999900";
    blueDim = "#0000b2";
    magentaDim = "#b200b2";
    cyanDim = "#00a6b2";
    whiteDim = "#bfbfbf";

    backgroundBright = "#000000";
    foregroundBright = "#b6b6b6";
    blackBright = "#666666";
    redBright = "#e50000";
    greenBright = "#00d900";
    yellowBright = "#e5e500";
    blueBright = "#0000ff";
    magentaBright = "#e500e5";
    cyanBright = "#00e5e5";
    whiteBright = "#e5e5e5";
  };
}

# OLD STUFF, OUT OF STANDARD:
/*
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

  # adapted from popping-and-locking and colors from vscode theme
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
  
  # don't remember, not black bg
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
*/
