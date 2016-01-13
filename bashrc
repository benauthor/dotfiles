# -*-Shell-Script-*-
#path
export PATH=~/local/bin/:~/.local/bin/:~/local/node/bin/:/usr/local/mysql/bin:/Library/Java/JavaVirtualMachines/jdk1.7.0_75.jdk/Contents/Home/bin:~/go/bin:~/.roswell/bin:$PATH

# history
export HISTFILESIZE=3000

# color prompt
export CLICOLOR=1
export LSCOLORS=Bxfxcxdxdxegedabxgacad

# git completion
#source /etc/bash_completion.d/git
source ~/local/git-completion.bash

function _update_ps1() {
    export PS1="$(~/local/powerline-shell/powerline-shell.py --mode flat $? 2> /dev/null)"
}

export PROMPT_COMMAND="_update_ps1; $PROMPT_COMMAND"


# ARCHFLAGS so things can compile
export ARCHFLAGS="-arch x86_64"

# nvm
[[ -s $HOME/.nvm/nvm.sh ]] && . $HOME/.nvm/nvm.sh

# let screen see current dir
if [ "$TERM" = "screen" ]; then
  screen_set_window_title () {
    local HPWD="$PWD"
    case $HPWD in
        $HOME) HPWD="~";;
        ## short name option:
        *) HPWD=`basename "$HPWD"`;;
    esac
    printf '\ek%s\e\\' "$HPWD"
  }
  PROMPT_COMMAND="screen_set_window_title; $PROMPT_COMMAND"
fi

# aliases
if [ -f ~/.bash_aliases ]; then
    source ~/.bash_aliases
fi

# shortcut to ssh via the gateway
s() {
  ssh -t gateway ssh $*
}

# svn
export SVN_EDITOR=vim

# java oh java
export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.7.0_75.jdk/Contents/Home

# ummmm
# export URBIT_HOME=/Users/bendere/work/urbit/urb

# for docker
export DOCKER_HOST=tcp://192.168.59.103:2376
export DOCKER_CERT_PATH=/Users/evanbender/.boot2docker/certs/boot2docker-vm
export DOCKER_TLS_VERIFY=1

# go
export GOPATH=$HOME/go

# devolate
export VM_MEMORY=3200
