scoop install aria2 7zip git
scoop config aria2-warning-enabled false
scoop install 
scoop bucket add extras
scoop bucket add nerd-fonts
scoop bucket add nonportable
scoop bucket add versions
scoop bucket add kress95 https://github.com/kress95/scoop-bucket
scoop bucket add insomnia https://github.com/insomnimus/scoop-bucket
scoop update

# cli
scoop install 7zip
scoop install 7zip19.00-helper
scoop install adb
scoop install bzip2
scoop install curl
# scoop install dark
scoop install deno
scoop install dprint
scoop install ffmpeg
scoop install gallery-dl
scoop install gcc
scoop install git
scoop install gzip
scoop install innounp
scoop install just
scoop install llvm
scoop install micro
scoop install mkvtoolnix
scoop install nodejs16
scoop install npiperelay
scoop install oh-my-posh
scoop install ps-dotenv
scoop install scoop-search
scoop install starship
scoop install sudo
scoop install tar
scoop install unzip
scoop install yt-dlp
scoop install zoxide

# apps
scoop install thorium-avx2-np
# scoop install taskbarx

# global apps
sudo scoop install --global equalizer-apo-np
sudo scoop install --global hide-volume-osd-np
sudo scoop install --global powertoys-np
sudo scoop install --global office-365-apps-minimal-np

# fonts
scoop install Delugia-Nerd-Font-Complete
scoop install Iosevka-NF-Mono
scoop install JetBrains-Mono
scoop install JetBrainsMono-NF-Mono
