#!/bin/sh

case $1 in
    "--up")
        pamixer --increase 5
        ;;
    "--down")
        pamixer --decrease 5    
        ;;
    "--mute")
        pamixer --toggle-mute
        ;;
esac

if [ "$(pamixer --get-volume-human)" = "muted" ]; then
    hook=1
else
    hook=2
fi

polybar-msg action audio hook $hook
