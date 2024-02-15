## system

- Configure 10 bit at `/etc/X11/xorg.conf.d/30-screen.conf`
- Maybe start only using nix for configs and devenvs?
  - [x] Drop programs.autorandr and services.autorandr
  - [x] Drop programs.git
  - [x] Drop programs.direnv configs
  - [x] Drop pacman.pkgs
  - [ ] Stop using home.packages and pkgs.*
