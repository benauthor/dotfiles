# -*-Shell-Script-*-
#path
export PATH=~/local/bin/:~/.local/bin/:~/local/node/bin/:/usr/local/mysql/bin:/Library/Java/JavaVirtualMachines/jdk1.8.0_51.jdk/Contents/Home/bin:~/go/bin:/Library/TeX/texbin:$PATH

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
# export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.7.0_75.jdk/Contents/Home
export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_51.jdk/Contents/Home

# ummmm
# export URBIT_HOME=/Users/bendere/work/urbit/urb

# go
export GOPATH=$HOME/go
export GO15VENDOREXPERIMENT=1

# devolate
export VM_MEMORY=5000

export DOCKER_MACHINE_NAME=default

prepare_docker() {
    docker-machine start $DOCKER_MACHINE_NAME
    eval $(docker-machine env $DOCKER_MACHINE_NAME)
    export DOCKER_IP=`docker-machine ip $DOCKER_MACHINE_NAME`
}

# prepare_docker
# echo "Hey, your docker machine $DOCKER_MACHINE_NAME is running on $DOCKER_IP"

# c can find homebrew-installed
export LIBRARY_PATH=/usr/local/lib/:$LIBRARY_PATH
export CPATH=/usr/local/include/:$CPATH

# java
export ECLIPSE_HOME=/Applications/Eclipse.app/Contents/Eclipse/
export GRADLE_HOME=/usr/local/Cellar/gradle/2.3/libexec

#
export DYLD_LIBRARY_PATH=/usr/local/opt/openssl/lib:$DYLD_LIBRARY_PATH

# nvm
export NVM_DIR="$HOME/.nvm"
. "$(brew --prefix nvm)/nvm.sh"
