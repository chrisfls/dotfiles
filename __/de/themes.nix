{ lib, ... }:
let
  absoluteBlack = "#000000";
  absoluteWhite = "#ffffff";
in
{
  options.theme = lib.mkOption { };
  options.themes = lib.mkOption { };
  config.themes = rec {
    popping-and-locking = {
      inherit absoluteBlack absoluteWhite;
      background = "#181921";
      foreground = "#EBDBB2";
      cursorColor = "#C7C7C7";
      selectionBackground = popping-and-locking.foreground;
      brightBlack = "#928374";
      brightWhite = popping-and-locking.foreground;
      brightRed = "#F42C3E";
      brightYellow = "#FABD2F";
      brightGreen = "#B8BB26";
      brightCyan = "#7EC16E";
      brightBlue = "#99C6CA";
      brightPurple = "#D3869B";
      black = "#1D2021";
      white = "#A89984";
      red = "#CC241D";
      yellow = "#D79921";
      green = "#98971A";
      cyan = "#689D6A";
      blue = "#458588";
      purple = "#B16286";
    };
    popping-and-locking-black = {
      inherit absoluteBlack absoluteWhite;
      inherit (popping-and-locking)
        brightBlack
        brightWhite
        brightRed
        brightYellow
        brightGreen
        brightCyan
        brightBlue
        brightPurple
        black
        white
        red
        yellow
        green
        cyan
        blue
        purple;
      background = "#000000";
      foreground = "#F2E5BC";
      cursorColor = popping-and-locking-black.foreground;
      selectionBackground = "#6A6B5D";
    };
  };
}
