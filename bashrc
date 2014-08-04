# -*-Shell-Script-*-
#path
export PATH=/usr/texbin:~/local\ applications/Racket\ v6.0/bin/:~/usr/local/heroku/bin:~/local/bin/:~/.local/bin/:~/local/node/bin/:~/.rvm/bin:/usr/local/mysql/bin:/Library/Java/JavaVirtualMachines/jdk1.7.0_40.jdk/Contents/Home/bin:$PATH

# history
export HISTFILESIZE=3000

# color prompt
export CLICOLOR=1
export LSCOLORS=Bxfxcxdxdxegedabxgacad

# git completion
#source /etc/bash_completion.d/git
source ~/.git-completion.bash

#powerline
# function _update_ps1() {
#    export PS1="$(~/local/powerline-bash/powerline-bash.py $?)"
# }
#
# export PROMPT_COMMAND="_update_ps1"

function _update_ps1() {
    export PS1="$(~/local/powerline-shell/powerline-shell.py --mode flat $? 2> /dev/null)"
}

export PROMPT_COMMAND="_update_ps1; $PROMPT_COMMAND"

#source ~/local/bash-powerline.sh

#ARCHFLAGS so things can compile
export ARCHFLAGS="-arch x86_64"

# load nvm, rvm, pythonbrew
[[ -s $HOME/.nvm/nvm.sh ]] && . $HOME/.nvm/nvm.sh
[[ -s $HOME/.rvm/scripts/rvm ]] && source $HOME/.rvm/scripts/rvm
# pythonbrew got abandoned
[[ -s $HOME/.pythonbrew/etc/bashrc ]] && source $HOME/.pythonbrew/etc/bashrc

#let screen see current dir
if [ "$TERM" = "screen" ]; then
  screen_set_window_title () {
    local HPWD="$PWD"
    case $HPWD in
        $HOME) HPWD="~";;

        ## long name option:
        # $HOME/*) HPWD="~${HPWD#$HOME}";;
        ## short name option:
        *) HPWD=`basename "$HPWD"`;;
    esac
    printf '\ek%s\e\\' "$HPWD"
  }
  PROMPT_COMMAND="screen_set_window_title; $PROMPT_COMMAND"
fi

#aliases
if [ -f ~/.bash_aliases ]; then
    source ~/.bash_aliases
fi

#purge the squid
function purge() { for i in {1..2};
                do squidclient -m PURGE -p 7000 -h
                    ashcache$i.usnews.com "$@"; done ;
                }

function publichtml {
    scp $1 sand1.usnews.com:~/public_html
}

export SVN_EDITOR=vim

# java oh java
export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.7.0_40.jdk/Contents/Home

# ummmm
export URBIT_HOME=/Users/bendere/work/urbit/urb

# for docker
export DOCKER_HOST=tcp://localhost:4243
