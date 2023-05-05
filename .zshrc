PROMPT='%~ $ '
eval "$(/usr/local/bin/brew shellenv)"
alias l="ls"
alias ll="ls -lhG"
alias lla="ls -lhaG"
alias ec="emacsclient -t -e '(when (get-buffer \"*eshell*\") (switch-to-buffer \"*eshell*\"))'"
alias ecd="emacsclient -t -s dev -e '(when (get-buffer \"*eshell*\") (switch-to-buffer \"*eshell*\"))'"
alias gitcfg='git --git-dir=$HOME/.cfg/.git/ --work-tree=$HOME'
alias rm="rm -i"
alias mv="mv -i"
export CPPFLAGS="-I/usr/local/opt/openjdk/include"

