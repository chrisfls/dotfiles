function dev --wraps='nix develop' --description 'run shell from /etc/nixos/shells/flake.nix'
  nix develop "/etc/nixos/shells#$argv" -c fish $argv[2..-1];
end
