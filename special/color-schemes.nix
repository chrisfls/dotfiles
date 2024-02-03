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
  # reminds me too much of windows, background not black
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
  # bad contrast with terrible blues
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
  # solves issues with xterm, but is too bright, no contrast changes
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
  # background not neutral, ok colors
  github-dark-high-contrast = {
    # Cursor colors
    # text = '#0a0c10'
    # cursor = '#f0f3f6'

    background = "#0a0c10";
    foreground = "#f0f3f6";
    black = "#7a828e";
    red = "#ff9492";
    green = "#26cd4d";
    yellow = "#f0b72f";
    blue = "#71b7ff";
    magenta = "#cb9eff";
    cyan = "#39c5cf";
    white = "#d9dee3";

    backgroundDim = "#0a0c10";
    foregroundDim = "#f0f3f6";
    blackDim = "#7a828e";
    redDim = "#ff9492";
    greenDim = "#26cd4d";
    yellowDim = "#f0b72f";
    blueDim = "#71b7ff";
    magentaDim = "#cb9eff";
    cyanDim = "#39c5cf";
    whiteDim = "#d9dee3";

    backgroundBright = "#0a0c10";
    foregroundBright = "#f0f3f6";
    blackBright = "#9ea7b3";
    redBright = "#ffb1af";
    greenBright = "#4ae168";
    yellowBright = "#f7c843";
    blueBright = "#91cbff";
    magentaBright = "#cb9eff";
    cyanBright = "#39c5cf";
    whiteBright = "#d9dee3";
  };
  popping-and-locking-black = {
    background = "#000000";
    foreground = "#F2E5BC";
    black = "#1D2021";
    red = "#CC241D";
    green = "#98971A";
    yellow = "#D79921";
    blue = "#458588";
    magenta = "#B16286";
    cyan = "#689D6A";
    white = "#A89984";

    backgroundDim = "#000000";
    foregroundDim = "#F2E5BC";
    blackDim = "#1D2021";
    redDim = "#CC241D";
    greenDim = "#98971A";
    yellowDim = "#D79921";
    blueDim = "#458588";
    magentaDim = "#B16286";
    cyanDim = "#689D6A";
    whiteDim = "#A89984";

    backgroundBright = "#000000";
    foregroundBright = "#F2E5BC";
    blackBright = "#928374";
    redBright = "#F42C3E";
    greenBright = "#B8BB26";
    yellowBright = "#FABD2F";
    blueBright = "#99C6CA";
    magentaBright = "#D3869B";
    cyanBright = "#7EC16E";
    whiteBright = "#EBDBB2";
  };
  vibrant-ink = {
    # Colors (VibrantInk)
    #   cursor:
    #     cursor: "#ffffff"
    #     text: "#000000"
    #   selection:
    #     background: "#b5d5ff"
    #     text: "#000000"
    background = "#000000";
    foreground = "#ffffff";
    black = "#878787";
    blue = "#44b4cc";
    cyan = "#44b4cc";
    green = "#ccff04";
    magenta = "#9933cc";
    red = "#ff6600";
    white = "#f5f5f5";
    yellow = "#ffcc00";

    backgroundDim = "#000000";
    foregroundDim = "#ffffff";
    blackDim = "#878787";
    blueDim = "#44b4cc";
    cyanDim = "#44b4cc";
    greenDim = "#ccff04";
    magentaDim = "#9933cc";
    redDim = "#ff6600";
    whiteDim = "#f5f5f5";
    yellowDim = "#ffcc00";

    backgroundBright = "#000000";
    foregroundBright = "#ffffff";
    blackBright = "#555555";
    blueBright = "#0000ff";
    cyanBright = "#00ffff";
    greenBright = "#00ff00";
    magentaBright = "#ff00ff";
    redBright = "#ff0000";
    whiteBright = "#e5e5e5";
    yellowBright = "#ffff00";
  };
  konsolas = {
    # cursor:
    #   cursor: '#c8c1c1'
    #   text: '#060606'
    # selection:
    #   background: '#060606'
    #   text: '#c8c1c1'

    background = "#060606";
    foreground = "#c8c1c1";
    black = "#000000";
    blue = "#2323a5";
    cyan = "#42b0c8";
    green = "#18b218";
    magenta = "#ad1edc";
    red = "#aa1717";
    white = "#c8c1c1";
    yellow = "#ebae1f";

    backgroundDim = "#060606";
    foregroundDim = "#c8c1c1";
    blackDim = "#000000";
    blueDim = "#2323a5";
    cyanDim = "#42b0c8";
    greenDim = "#18b218";
    magentaDim = "#ad1edc";
    redDim = "#aa1717";
    whiteDim = "#c8c1c1";
    yellowDim = "#ebae1f";

    backgroundBright = "#060606";
    foregroundBright = "#c8c1c1";
    blackBright = "#7b716e";
    blueBright = "#4b4bff";
    cyanBright = "#69ffff";
    greenBright = "#5fff5f";
    magentaBright = "#ff54ff";
    redBright = "#ff4141";
    whiteBright = "#ffffff";
    yellowBright = "#ffff55";
  };
  paul-millr = {
    # cursor:
    #   cursor = "#4d4d4d";
    #   text = "#ffffff";
    # selection:
    #   background = "#414141";
    #   text = "#ffffff";

    background = "#000000";
    foreground = "#f2f2f2";
    black = "#2a2a2a";
    blue = "#396bd7";
    cyan = "#66ccff";
    green = "#79ff0f";
    magenta = "#b449be";
    red = "#ff0000";
    white = "#bbbbbb";
    yellow = "#e7bf00";

    backgroundDim = "#000000";
    foregroundDim = "#f2f2f2";
    blackDim = "#2a2a2a";
    blueDim = "#396bd7";
    cyanDim = "#66ccff";
    greenDim = "#79ff0f";
    magentaDim = "#b449be";
    redDim = "#ff0000";
    whiteDim = "#bbbbbb";
    yellowDim = "#e7bf00";

    backgroundBright = "#000000";
    foregroundBright = "#f2f2f2";
    blackBright = "#666666";
    blueBright = "#709aed";
    cyanBright = "#7adff2";
    greenBright = "#66ff66";
    magentaBright = "#db67e6";
    redBright = "#ff0080";
    whiteBright = "#ffffff";
    yellowBright = "#f3d64e";
  };
}

# PENDING STUFF

/*
  # Colors (iTerm2 Default)
  colors:
  bright:
  black: '#686868'
  blue: '#6871ff'
  cyan: '#60fdff'
  green: '#5ffa68'
  magenta: '#ff77ff'
  red: '#ff6e67'
  white: '#ffffff'
  yellow: '#fffc67'
  cursor:
  cursor: '#e5e5e5'
  text: '#000000'
  normal:
  black: '#000000'
  blue: '#2225c4'
  cyan: '#00c5c7'
  green: '#00c200'
  magenta: '#ca30c7'
  red: '#c91b00'
  white: '#ffffff'
  yellow: '#c7c400'
  primary:
  background: '#000000'
  foreground: '#ffffff'
  selection:
  background: '#c1deff'
  text: '#000000'
  # Colors (3024 Night)
  colors:
  bright:
  black: '#5c5855'
  blue: '#807d7c'
  cyan: '#cdab53'
  green: '#3a3432'
  magenta: '#d6d5d4'
  red: '#e8bbd0'
  white: '#f7f7f7'
  yellow: '#4a4543'
  cursor:
  cursor: '#a5a2a2'
  text: '#090300'
  normal:
  black: '#090300'
  blue: '#01a0e4'
  cyan: '#b5e4f4'
  green: '#01a252'
  magenta: '#a16a94'
  red: '#db2d20'
  white: '#a5a2a2'
  yellow: '#fded02'
  primary:
  background: '#090300'
  foreground: '#a5a2a2'
  selection:
  background: '#4a4543'
  text: '#a5a2a2'
  # Colors (CGA)
  colors:
  bright:
  black: '#555555'
  blue: '#5555ff'
  cyan: '#55ffff'
  green: '#55ff55'
  magenta: '#ff55ff'
  red: '#ff5555'
  white: '#ffffff'
  yellow: '#ffff55'
  cursor:
  cursor: '#b8b8b8'
  text: '#ffffff'
  normal:
  black: '#000000'
  blue: '#0000aa'
  cyan: '#00aaaa'
  green: '#00aa00'
  magenta: '#aa00aa'
  red: '#aa0000'
  white: '#aaaaaa'
  yellow: '#aa5500'
  primary:
  background: '#000000'
  foreground: '#aaaaaa'
  selection:
  background: '#c1deff'
  text: '#000000'
  # Colors (Dark Pastel)
  colors:
  bright:
  black: '#555555'
  blue: '#5555ff'
  cyan: '#55ffff'
  green: '#55ff55'
  magenta: '#ff55ff'
  red: '#ff5555'
  white: '#ffffff'
  yellow: '#ffff55'
  cursor:
  cursor: '#bbbbbb'
  text: '#ffffff'
  normal:
  black: '#000000'
  blue: '#5555ff'
  cyan: '#55ffff'
  green: '#55ff55'
  magenta: '#ff55ff'
  red: '#ff5555'
  white: '#bbbbbb'
  yellow: '#ffff55'
  primary:
  background: '#000000'
  foreground: '#ffffff'
  selection:
  background: '#b5d5ff'
  text: '#000000'
  # Colors (deep)
  colors:
  bright:
  black: '#535353'
  blue: '#9fa9ff'
  cyan: '#8df9ff'
  green: '#22ff18'
  magenta: '#e09aff'
  red: '#fb0007'
  white: '#ffffff'
  yellow: '#fedc2b'
  cursor:
  cursor: '#d0d0d0'
  text: '#151515'
  normal:
  black: '#000000'
  blue: '#5665ff'
  cyan: '#50d2da'
  green: '#1cd915'
  magenta: '#b052da'
  red: '#d70005'
  white: '#e0e0e0'
  yellow: '#d9bd26'
  primary:
  background: '#090909'
  foreground: '#cdcdcd'
  selection:
  background: '#780002'
  text: '#ececec'
  # Colors (Firefly Traditional)
  colors:
  bright:
  black: '#828282'
  blue: '#838dff'
  cyan: '#29f0f0'
  green: '#2ee720'
  magenta: '#ff5cfe'
  red: '#ff3b1e'
  white: '#ebebeb'
  yellow: '#ecec16'
  cursor:
  cursor: '#00f900'
  text: '#ffffff'
  normal:
  black: '#000000'
  blue: '#5a63ff'
  cyan: '#33bbc7'
  green: '#33bc26'
  magenta: '#d53ad2'
  red: '#c23720'
  white: '#cccccc'
  yellow: '#afad24'
  primary:
  background: '#000000'
  foreground: '#f5f5f5'
  selection:
  background: '#cfeac6'
  text: '#000000'
  # Colors (Konsolas)
  colors:
  bright:
  black: '#7b716e'
  blue: '#4b4bff'
  cyan: '#69ffff'
  green: '#5fff5f'
  magenta: '#ff54ff'
  red: '#ff4141'
  white: '#ffffff'
  yellow: '#ffff55'
  cursor:
  cursor: '#c8c1c1'
  text: '#060606'
  normal:
  black: '#000000'
  blue: '#2323a5'
  cyan: '#42b0c8'
  green: '#18b218'
  magenta: '#ad1edc'
  red: '#aa1717'
  white: '#c8c1c1'
  yellow: '#ebae1f'
  primary:
  background: '#060606'
  foreground: '#c8c1c1'
  selection:
  background: '#060606'
  text: '#c8c1c1'
  # Colors (Mathias)
  colors:
  bright:
  black: '#555555'
  blue: '#5555ff'
  cyan: '#55ffff'
  green: '#55ff55'
  magenta: '#ff55ff'
  red: '#ff5555'
  white: '#ffffff'
  yellow: '#ffff55'
  cursor:
  cursor: '#bbbbbb'
  text: '#ffffff'
  normal:
  black: '#000000'
  blue: '#c48dff'
  cyan: '#67d9f0'
  green: '#a6e32d'
  magenta: '#fa2573'
  red: '#e52222'
  white: '#f2f2f2'
  yellow: '#fc951e'
  primary:
  background: '#000000'
  foreground: '#bbbbbb'
  selection:
  background: '#555555'
  text: '#f2f2f2'
  # Colors (Monokai Vivid)
  colors:
  bright:
  black: '#838383'
  blue: '#0443ff'
  cyan: '#51ceff'
  green: '#b1e05f'
  magenta: '#f200f6'
  red: '#f6669d'
  white: '#ffffff'
  yellow: '#fff26d'
  cursor:
  cursor: '#fb0007'
  text: '#ea0009'
  normal:
  black: '#121212'
  blue: '#0443ff'
  cyan: '#01b6ed'
  green: '#98e123'
  magenta: '#f800f8'
  red: '#fa2934'
  white: '#ffffff'
  yellow: '#fff30a'
  primary:
  background: '#121212'
  foreground: '#f9f9f9'
  selection:
  background: '#ffffff'
  text: '#000000'
  # Colors (PaulMillr)
  colors:
  bright:
  black: '#666666'
  blue: '#709aed'
  cyan: '#7adff2'
  green: '#66ff66'
  magenta: '#db67e6'
  red: '#ff0080'
  white: '#ffffff'
  yellow: '#f3d64e'
  cursor:
  cursor: '#4d4d4d'
  text: '#ffffff'
  normal:
  black: '#2a2a2a'
  blue: '#396bd7'
  cyan: '#66ccff'
  green: '#79ff0f'
  magenta: '#b449be'
  red: '#ff0000'
  white: '#bbbbbb'
  yellow: '#e7bf00'
  primary:
  background: '#000000'
  foreground: '#f2f2f2'
  selection:
  background: '#414141'
  text: '#ffffff'
  # Colors (Pro)
  colors:
  bright:
  black: '#666666'
  blue: '#0000ff'
  cyan: '#00e5e5'
  green: '#00d900'
  magenta: '#e500e5'
  red: '#e50000'
  white: '#e5e5e5'
  yellow: '#e5e500'
  cursor:
  cursor: '#4d4d4d'
  text: '#ffffff'
  normal:
  black: '#000000'
  blue: '#2009db'
  cyan: '#00a6b2'
  green: '#00a600'
  magenta: '#b200b2'
  red: '#990000'
  white: '#bfbfbf'
  yellow: '#999900'
  primary:
  background: '#000000'
  foreground: '#f2f2f2'
  selection:
  background: '#414141'
  text: '#000000'
  # Colors (Symfonic)
  colors:
  bright:
  black: '#1b1d21'
  blue: '#0084d4'
  cyan: '#ccccff'
  green: '#56db3a'
  magenta: '#b729d9'
  red: '#dc322f'
  white: '#ffffff'
  yellow: '#ff8400'
  cursor:
  cursor: '#dc322f'
  text: '#ffffff'
  normal:
  black: '#000000'
  blue: '#0084d4'
  cyan: '#ccccff'
  green: '#56db3a'
  magenta: '#b729d9'
  red: '#dc322f'
  white: '#ffffff'
  yellow: '#ff8400'
  primary:
  background: '#000000'
  foreground: '#ffffff'
  selection:
  background: '#073642'
  text: '#ffffff'
  # Colors (iTerm2 Tango Dark)
  colors:
  bright:
  black: '#686a66'
  blue: '#84b0d8'
  cyan: '#37e6e8'
  green: '#99e343'
  magenta: '#bc94b7'
  red: '#f54235'
  white: '#f1f1f0'
  yellow: '#fdeb61'
  cursor:
  cursor: '#ffffff'
  text: '#000000'
  normal:
  black: '#000000'
  blue: '#427ab3'
  cyan: '#00a7aa'
  green: '#5ea702'
  magenta: '#89658e'
  red: '#d81e00'
  white: '#dbded8'
  yellow: '#cfae00'
  primary:
  background: '#000000'
  foreground: '#ffffff'
  selection:
  background: '#c1deff'
  text: '#000000'
*/



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
