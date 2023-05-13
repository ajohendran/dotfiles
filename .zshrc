PROMPT='%~ $ '
eval "$(/usr/local/bin/brew shellenv)"
alias l="ls"
alias ll="ls -lhG"
alias lla="ls -lhaG"
alias ec="emacsclient -t "
alias ecd="emacsclient -t -s dev"
alias cfgit='git --git-dir=$HOME/.cfg/.git/ --work-tree=$HOME'
alias rm="rm -i"
alias mv="mv -i"
export CPPFLAGS="-I/usr/local/opt/openjdk/include"

