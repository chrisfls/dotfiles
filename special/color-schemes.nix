let
  absoluteBlack = "#000000";
  absoluteWhite = "#ffffff";
in
rec {
  popping-and-locking = {
    inherit absoluteBlack absoluteWhite;
    background = "#181921";
    foreground = "#EBDBB2";
    cursorColor = "#C7C7C7";
    selectionBackground = popping-and-locking.foreground;
    blackBright = "#928374";
    whiteBright = popping-and-locking.foreground;
    redBright = "#F42C3E";
    yellowBright = "#FABD2F";
    greenBright = "#B8BB26";
    cyanBright = "#7EC16E";
    blueBright = "#99C6CA";
    magentaBright = "#D3869B";
    black = "#1D2021";
    white = "#A89984";
    red = "#CC241D";
    yellow = "#D79921";
    green = "#98971A";
    cyan = "#689D6A";
    blue = "#458588";
    magenta = "#B16286";
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
}
