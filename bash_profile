#!/bin/bash

if [ -f "/Library/Developer/CommandLineTools/usr/share/git-core/git-prompt.sh" ]; then
    source /Library/Developer/CommandLineTools/usr/share/git-core/git-prompt.sh
else
    __git_ps1=true
fi

function __ps1_bgcolor() {
    local prev_cmd_exit="$?"

    case "$prev_cmd_exit" in
        "0" )
            BG_COLOR=2 ;;
        * )
            BG_COLOR=1 ;;
    esac
    printf "$(tput setab $BG_COLOR)$(tput setaf 0)"
}

export EDITOR=e
export TERM=xterm
export PYTHONSTARTUP="$HOME/.pythonstartup.py"
export REPOPATH="$HOME/repos/"
export SRCPATH="$HOME/src"
export GOPATH="$HOME"
export PATH="/opt/emacs/bin:/usr/local/bin:/usr/local/sbin:$PATH"
export PATH="$HOME/bin:$REPOPATH/toolbox:$SRCPATH/go/bin:$PATH"
export PS1="\$(__ps1_bgcolor) \u@\h:\w $(tput setab 4)$(tput setaf 7)$(tput sgr0)\n--> \$(__git_ps1) "

if [ "x$(uname -s)" == "xDarwin" ]; then
    alias ls='ls -G -F'
else
    alias ls='ls --color -F'
fi
alias ll='ls -l'
alias la='ll -a'
alias tm='tmux attach -t main || tmux new -s main'

if [ -f $HOME/repos/awstools/awshelpers.sh ]; then
    source $HOME/repos/awstools/awshelpers.sh
fi
