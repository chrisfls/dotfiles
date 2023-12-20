#!/bin/bash

if /usr/bin/bluetoothctl show | grep -q "Powered: yes"; then
    hook=2
else
    hook=1
fi

if [ "$1" = "--toggle" ]; then
    if [ "$hook" = "2" ]; then
        /usr/bin/bluetoothctl power off
        hook=1
    else
        /usr/bin/bluetoothctl power on
        hook=2
    fi
fi


polybar-msg action bluetooth hook $hook
