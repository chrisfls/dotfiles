#!/bin/bash

if [ "$(id -u)" = "0" ]; then
  echo "--- ROOT SETUP ---"

  set -x

  # change hostname
  echo "[network]" >> /etc/wsl.conf
  echo "hostname = arch-wsl-rmxp" >> /etc/wsl.conf
  
  # enable sudo for wheel group
  echo "%wheel ALL=(ALL) ALL" > /etc/sudoers.d/wheel

  # install chaotic aur
  pacman-key --recv-key FBA220DFC880C036 --keyserver keyserver.ubuntu.com
  pacman-key --lsign-key FBA220DFC880C036
  pacman -U 'https://cdn-mirror.chaotic.cx/chaotic-aur/chaotic-keyring.pkg.tar.zst' 'https://cdn-mirror.chaotic.cx/chaotic-aur/chaotic-mirrorlist.pkg.tar.zst'
  echo "" >> /etc/pacman.conf
  echo "[chaotic-aur]" >> /etc/pacman.conf
  echo "Include = /etc/pacman.d/chaotic-mirrorlist" >> /etc/pacman.conf

  # enable pacman progress bar
  sed -i '/# Misc options/a ILoveCandy' /etc/pacman.conf

  # update archlinux keyring and install powerpill
  pacman -Sy archlinux-keyring chaotic-keyring && pacman -Syu

  # install packages:
  # - nix (permantently)
  # - dconf (permantently)
  # - pacman-contrib (permanently)
  # - powerpill (permanently)
  # - git (temporarely), needed to clone nix-configs
  # - openssh (temporarely), needed to clone nix-configs
  # - cloudflare-warp-bin (permanently)
  # - docker (permanently)
  # - docker-compose (permanently)
  # - chromium (permanently), needed to configure warp
  pacman -Syu chromium cloudflare-warp-bin dconf docker docker-compose git nix openssh pacman-contrib powerpill

  # TODO: fix datetime with https://unix.stackexchange.com/questions/737365/cant-override-a-systemd-units-conditionvirtualization-on-archlinux-on-distrod

  # enable warp
  systemctl enable --now warp-svc.service

  # configure nix
  systemctl enable docker.service
  systemctl enable nix-daemon.service
  echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf

  nix-channel --add https://nixos.org/channels/nixos-22.11 nixpkgs
  nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs-unstable
  nix-channel --update

  # setup root password
  passwd

  # create chris user
  useradd -m -G wheel,docker,nix-users -s /bin/bash chris
  echo "trusted-users = root chris" | tee -a /etc/nix/nix.conf

  # setup chris password
  passwd chris

  # make folder for nix config and make chris own it
  mkdir /etc/nixos
  chown -R chris /etc/nixos

  set +x
  
  echo "--- ROOT SETUP DONE ---"
  echo ""
  echo "Set up the default user on windows:"
  echo ""
  echo "\> Arch.exe config --default-user chris"
  echo ""
  echo "If the default user has not been changed, restart the LxssManager in an Admin command prompt:"
  echo ""
  echo "\> net stop lxssmanager && net start lxssmanager"
  echo ""
  echo "Restart wsl and add your ssh keys to '/home/chris/.ssh/id_ed25519'."
  echo "After that, run this script again."
else
  echo "--- USER SETUP ---"

  set -x

  # load ssh keys
  sudo chown chris ~/.ssh/id_ed25519
  sudo chown chris ~/.ssh/id_ed25519.pub
  chmod 400 ~/.ssh/id_ed25519
  eval "$(ssh-agent -s)"
  ssh-add ~/.ssh/id_ed25519

  # clone nix config
  git clone git@github.com:chrisfls/nix-configs.git /etc/nixos

  # install home manager
  nix-channel --add https://nixos.org/channels/nixos-22.11 nixpkgs
  nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs-unstable
  nix-channel --add https://github.com/nix-community/home-manager/archive/release-22.11.tar.gz home-manager
  nix-channel --update
  nix-shell '<home-manager>' -A install

  # apply home manager config
  home-manager switch --flake '/etc/nixos'

  # uninstall temporary packages
  sudo pacman -Rsnc git openssh

  set +x

  echo "--- USER SETUP DONE ---"
  echo ""
  echo "To fix the error “warning: Nix search path entry '/nix/var/nix/profiles/per-user/$USER/channels' does not exist, ignoring”"
  echo "Run the following command as root:"
  echo ""
  echo "$ nix-channel --update"
  echo ""
  echo "Refer to 'https://wiki.archlinux.org/title/Nix' for help with other issues."
  echo ":)"
fi
