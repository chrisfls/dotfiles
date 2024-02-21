- `$ systemctl enable systemd-timesyncd.service`

Files:

- `/etc/X11/xorg.conf.d/30-monitor.conf` come from [here](https://wiki.archlinux.org/title/GPD_Win_Max)
- `/etc/X11/xorg.conf.d/20-intel.conf` allows having vsync off with no tearing

Not working:

- `/etc/X11/xorg.conf.d/30-monitor.conf`
- `/etc/pam.d/i3lock`


Home setup

```sh
sudo mkdir -m 0755 -p /nix/var/nix/{profiles,gcroots}/per-user/$USER
```
