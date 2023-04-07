scoop install emacs ripgrep fd llvm
git clone https://github.com/hlissner/doom-emacs $env:APPDATA/.emacs.d
cd $env:APPDATA/.emacs.d
git checkout 22097b5a755a5b1d661e362a8441b61e37f777c9
& "$(scoop prefix git)\bin\bash.exe" -c '
export PATH="$PATH:$HOME/AppData/Roaming/.emacs.d/bin"
doom install
doom sync
'
