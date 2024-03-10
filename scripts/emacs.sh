#!/bin/bash

mkdir -p "$XDG_CONFIG_HOME/doom"
ln -s "$(realpath "./assets/doom/config.el")" "$XDG_CONFIG_HOME/doom/config.el"
ln -s "$(realpath "./assets/doom/init.el")" "$XDG_CONFIG_HOME/doom/init.el"
ln -s "$(realpath "./assets/doom/packages.el")" "$XDG_CONFIG_HOME/doom/packages.el"

mkdir -p "$XDG_CONFIG_HOME/emacs"
git init "$XDG_CONFIG_HOME/emacs"
git -C "$XDG_CONFIG_HOME/emacs" remote add origin git@github.com:doomemacs/doomemacs.git
git -C "$XDG_CONFIG_HOME/emacs" fetch origin "d657be1744a1481dc4646d0b62d5ee1d3e75d1d8"
git -C "$XDG_CONFIG_HOME/emacs" reset --hard FETCH_HEAD
"$XDG_CONFIG_HOME/emacs/bin/doom" build
