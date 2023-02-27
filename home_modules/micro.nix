# commong fish settings
{ pkgs, ... }:
{
  programs.micro = {
    enable = true;
    settings = {
      colorscheme = "solarized-tc";
    };
  };
}
