### bspwm

- Fix opacity for monocle/fullscreen apps in bspwm
- Add [swallow](https://github.com/JopStro/bspswallow) or alternative

#### low priority

- Fix betterlockscreen (deferred because this is a desktop...)
- Add `--layer` keybindings? (below/normal/above)

### systemd

- Fix cameractrls service
- Migrate rclone service to home-manager (and ensure it is working)
- Fix udiskie service
- Fix xsettingsd service

### apps

- Configure `pkgs.mpv`
- Add `PWA` for `languagetool`
- Migrate `vscode` settings to `codium` thru `home-manager`
- Configure `pkgs.helix`
- Hide rofi icons from main menu
  - I really tried, but it's not working...
- Make tray apps restore to current workspace

### system

- Configure 10 bit at `/etc/X11/xorg.conf.d/30-screen.conf`
