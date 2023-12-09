set fish_greeting ""

if not set -q tide_setup
  echo "tide setup init"
  echo 1 2 1 2 3 2 1 1 y | tide configure >/dev/null
  set --universal tide_character_icon "λ"
  set --universal tide_setup true
  echo "tide setup done"
end

any-nix-shell fish --info-right | source
