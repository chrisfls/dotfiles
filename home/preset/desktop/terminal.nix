#
# Performance ranking using github.com/cmuratori/termbench
#
#  1st foot
#
#    parser:   fastest 
#    render:   fast cpu render
#    latency:  low
#    features: lots, but no ligatures 
#    protocol: wayland
#
#  2nd zutty
#
#    parser:   fast
#    render:   fast gpu render
#    latency:  low
#    features: few
#    protocol: both
#
#  3rd alacritty
#
#    parser:   fast (slower than foot)
#    render:   fast gpu render
#    latency:  mid
#    features: few
#    protocol: both + windows
#
#  4th contour
#
#    parser:   fast like alacritty
#    render:   fast gpu render
#    latency:  unknown
#    features: lots
#    protocol: both
#
#  5th kitty
#
#    parser:   slower than contour
#    render:   fast gpu render
#    latency:  unknown
#    features: lots
#    protocol: both + windows
#
#  6th wezterm
#
#    parser:   lot slower than kitty
#    render:   gpu render
#    latency:  unknown
#    features: most of all
#    protocol: both + windows
#
{ pkgs, ... }:
{
  home.packages = [ pkgs.contour ];

  extra.nixGL.overlay.contour = [ "contour" ];

  services.sxhkd.keybindings."super + semicolon" = "contour";
}
