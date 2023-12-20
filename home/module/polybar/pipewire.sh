#!/bin/sh

case $1 in
    "--up")
        /home/kress/.nix-profile/bin/pamixer --increase 5
        ;;
    "--down")
        /home/kress/.nix-profile/bin/pamixer --decrease 5    
        ;;
    "--mute")
        /home/kress/.nix-profile/bin/pamixer --toggle-mute
        ;;
esac

if [ "$(/home/kress/.nix-profile/bin/pamixer --get-volume-human)" = "muted" ]; then
    hook=1
else
    hook=2
fi

polybar-msg action audio hook $hook
