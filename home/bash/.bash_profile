# generic shell settings
[[ -f ~/.bashrc ]] && . ~/.bashrc

# interactive shell settings
export TERM="xterm-256color";
export COLORTERM=truecolor
export MICRO_TRUECOLOR=1

unset HISTFILE

# load generated interactive shell settings
[[ -f ~/.profile ]] && . ~/.profile
