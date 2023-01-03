```sh
git clone --recurse-submodules --bare git@github.com:kress95/dotfiles.git $HOME/.system.git
git --git-dir=$HOME/.system.git --work-tree=/etc/nixos checkout --recurse-submodules
```
