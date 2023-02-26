```sh
git clone --recurse-submodules --bare git@github.com:kress95/nix-configs.git $HOME/.system.git
git --git-dir=$HOME/.system.git --work-tree=/etc/nixos checkout --recurse-submodules
```
