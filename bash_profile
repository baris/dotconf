
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

export PYTHONSTARTUP=$HOME/.pythonstartup.py
export GOROOT="$HOME/src/go"

export PATH="/usr/local/bin:/usr/local/sbin:$PATH:$HOME/repos/toolbox:$GOROOT/bin"
export PS1="\$(__ps1_bgcolor) \u@\h:\w $(tput setab 4)$(tput setaf 7) ✣ $(tput sgr0)\$(__git_ps1) "

export EDITOR=e

alias ls='ls -G'
alias ll='ls -l'
alias la='ll -a'

source $HOME/repos/awstools/awshelpers.sh
