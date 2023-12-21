let
  absoluteBlack = "#000000";
  absoluteWhite = "#ffffff";
in
rec {
  popping-and-locking = {
    inherit absoluteBlack absoluteWhite;
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
  popping-and-locking-black = {
    inherit absoluteBlack absoluteWhite;
    inherit (popping-and-locking)
      black
      blackBright
      blue
      blueBright
      cyan
      cyanBright
      green
      greenBright
      magenta
      magentaBright
      red
      redBright
      white
      whiteBright
      yellow
      yellowBright;
    background = "#000000";
    foreground = "#F2E5BC";
    cursorColor = popping-and-locking-black.foreground;
    selectionBackground = "#6A6B5D";
  };
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
}
