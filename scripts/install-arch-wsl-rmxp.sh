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
  # - chromium (permanently), needed to configure warp
  pacman -Syu nix dconf pacman-contrib powerpill git openssh chromium cloudflare-warp-bin

  # TODO: fix datetime with https://unix.stackexchange.com/questions/737365/cant-override-a-systemd-units-conditionvirtualization-on-archlinux-on-distrod

  # enable warp
  systemctl enable --now warp-svc.service

  # configure nix
  systemctl enable nix-daemon.service
  echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf
  echo "export NIX_PATH=/nix/var/nix/profiles/per-user/root/channels" >> /etc/bash.bashrc
  nix-channel --add https://nixos.org/channels/nixpkgs-unstable
  nix-channel --update

  # setup root password
  passwd

  # create kress user
  useradd -m -G wheel,nix-users -s /bin/bash kress
  echo "trusted-users = root kress" | tee -a /etc/nix/nix.conf

  # setup kress password
  passwd kress

  # make folder for nix config and make kress own it
  mkdir /etc/nixos
  chown -R kress /etc/nixos

  set +x
  
  echo "--- ROOT SETUP DONE ---"
  echo ""
  echo "Set up the default user on windows:"
  echo ""
  echo "\> Arch.exe config --default-user kress"
  echo ""
  echo "If the default user has not been changed, restart the LxssManager in an Admin command prompt:"
  echo ""
  echo "\> net stop lxssmanager && net start lxssmanager"
  echo ""
  echo "Restart wsl and add your ssh keys to '/home/kress/.ssh/id_ed25519'."
  echo "After that, run this script again."
else
  echo "--- USER SETUP ---"

  set -x

  # load ssh keys
  sudo chown kress ~/.ssh/id_ed25519
  sudo chown kress ~/.ssh/id_ed25519.pub
  chmod 400 ~/.ssh/id_ed25519
  eval "$(ssh-agent -s)"
  ssh-add ~/.ssh/id_ed25519

  # clone nix config
  git clone git@github.com:kress95/nix-configs.git /etc/nixos

  # install home manager
  nix-channel --add https://nixos.org/channels/nixpkgs-unstable
  nix-channel --add "https://github.com/nix-community/home-manager/archive/master.tar.gz" home-manager
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
