let ssot = import ./common/ssot.nix; in
{
  "home/dev/dot-envrc.secret.age".publicKeys = [ ssot.keys.wsl.kress ssot.keys.arch-rmxp.kress ];
  "home/dev/dot-npmrc.age".publicKeys = [ ssot.keys.wsl.kress ssot.keys.arch-rmxp.kress ];
  "home/work/paack/dot-envrc.secret.age".publicKeys = [ ssot.keys.wsl.kress ssot.keys.arch-rmxp.kress ];
}
