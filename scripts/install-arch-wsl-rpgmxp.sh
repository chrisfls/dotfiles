#!/bin/bash

if [ "$(id -u)" = "0" ]; then
  echo "--- ROOT SETUP ---"

  set -x

  echo "[network]" >> /etc/hostname
  echo "hostname = arch-wsl-rpgmxp" >> /etc/hostname
  
  echo "%wheel ALL=(ALL) ALL" > /etc/sudoers.d/wheel

  pacman -Sy archlinux-keyring && pacman -Su
  pacman -Syu nix git

  systemctl enable nix-daemon.service
  echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf
  echo "export NIX_PATH=/nix/var/nix/profiles/per-user/root/channels" >> /etc/bash.bashrc

  passwd

  useradd -m -G wheel,nix-users -s /bin/bash kress
  echo "trusted-users = root kress" | sudo tee -a /etc/nix/nix.conf

  passwd kress

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
  echo "Restart wsl and install your ssh keys at '/home/kress/.ssh'."
  echo "After that, run this script again."
else
  echo "--- USER SETUP ---"

  set -x

  chmod 400 ~/.ssh/id_ed25519
  eval "$(ssh-agent -s)"
  ssh-add ~/.ssh/id_ed25519

  git clone --recurse-submodules git@github.com:kress95/nix-configs.git /etc/nixos

  mv /etc/nixos/.git ~/.system.git

  nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs-unstable
  nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
  nix-channel --update

  nix-shell '<home-manager>' -A install

  set +x

  echo "--- USER SETUP DONE ---"
  echo ""
  echo "Now run:"
  echo ""
  echo "$ home-manager switch --flake '/etc/nixos'"
  echo ""
  echo ""
  echo "If everything went fine you can uninstall git with:"
  echo ""
  echo "$ pacman -Rsnc git"
  echo ""
  echo "To finish the setup."
fi
