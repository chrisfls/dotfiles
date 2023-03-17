# TODO: embed this in fish config
set fish_greeting ""

if not set -q tide_setup
  echo "tide setup init"
  echo 1 2 1 1 2 1 y | tide configure >/dev/null
  set --universal tide_character_icon "λ"
  set --universal tide_setup true
  echo "tide setup done"
end

if not set -q git_setup
  echo "ssh setup init"
  if not test -f $HOME/.ssh/id_ed25519
    echo generating $HOME/.ssh/id_ed25519
    echo y | ssh-keygen -t ed25519 -C "$USER@$hostname" -N '' -f $HOME/.ssh/id_ed25519
  else
    echo $HOME/.ssh/id_ed25519 already exists
  end
  set --universal git_setup true
  echo "ssh setup done"
end

# if not set -q warp_setup
#   # I wanted to copy stuff from /var/lib/cloudflare-warp to stop this ugly
#   # setup, but I don't want to change the cloudflare-warp.nix package...
#   echo "cloudflare warp setup init"
#   warp-cli register
#   warp-cli connect
#   warp-cli register
#   warp-cli teams-enroll paack
#   set --universal warp_setup true
#   echo "NOTE: Running curl https://www.cloudflare.com/cdn-cgi/trace/"
#   echo "      will still report that warp is off, don't fret over this."
#   echo "      For now, what matters is that git works :)"
#   echo "cloudflare warp setup done"
# end

any-nix-shell fish --info-right | source
direnv hook fish | source
