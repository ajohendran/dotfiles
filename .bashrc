# .bashrc

### AWS Amazon Linux default stuff

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]
then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
if [ -d ~/.bashrc.d ]; then
	for rc in ~/.bashrc.d/*; do
		if [ -f "$rc" ]; then
			. "$rc"
		fi
	done
fi

unset rc


### Aravindh customizations

alias ll='ls -lh'
alias lla='ls -lha'
alias cfgit='/usr/bin/git --git-dir=$HOME/.cfg/.git --work-tree=$HOME'
alias rm='rm -i'
alias mv='mv -i'

export CPPFLAGS='-I/usr/local/opt/openjdk/include'

