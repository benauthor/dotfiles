# -*-Shell-Script-*-
#path
export PATH=~/local/bin/:~/.local/bin/:/Library/Java/JavaVirtualMachines/jdk1.8.0_51.jdk/Contents/Home/bin:~/go/bin:/Library/TeX/texbin:$PATH

# history
export HISTFILESIZE=3000

# color prompt
export CLICOLOR=1
export LSCOLORS=Bxfxcxdxdxegedabxgacad

# git
source /usr/local/etc/bash_completion.d/git-completion.bash
source /usr/local/etc/bash_completion.d/git-prompt.sh

# function _update_ps1() {
#     export PS1="$(~/local/powerline-shell/powerline-shell.py --mode patched $? 2> /dev/null)"
# }
# export PROMPT_COMMAND="_update_ps1; $PROMPT_COMMAND"

__prompt_command() {
    local EXIT="$?"             # This needs to be first
    PS1=""

    local RCol='\[\e[0m\]'

    local Red='\[\e[0;31m\]'
    local Blu='\[\e[0;34m\]'
    # local DimLBlu='\[\e[2;34m\]'
    local LBlu='\[\e[0;94m\]'
    local LCy='\[\e[0;96m\]'

    PS1+="${LCy}\u@\h:${LBlu}\W${Blu}$(__git_ps1)"


    if [ $EXIT != 0 ]; then
        PS1+="${Red}$ ${RCol}"      # Add red if exit code non 0
    else
        PS1+="${LCy}$ ${RCol}"
    fi
}
PROMPT_COMMAND=__prompt_command # Func to gen PS1 after CMDs

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
# devolate
export VM_MEMORY=5000

# java
export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_51.jdk/Contents/Home
export ECLIPSE_HOME=/Applications/Eclipse.app/Contents/Eclipse/
export GRADLE_HOME=/usr/local/Cellar/gradle/2.3/libexec

# go
export GOPATH=$HOME/go

# docker
export DOCKER_MACHINE_NAME=default

prepare_docker() {
    docker-machine start $DOCKER_MACHINE_NAME
    eval $(docker-machine env $DOCKER_MACHINE_NAME)
    export DOCKER_IP=`docker-machine ip $DOCKER_MACHINE_NAME`
}
# prepare_docker
# echo "Hey, your docker machine $DOCKER_MACHINE_NAME is running on $DOCKER_IP"

# c
# ARCHFLAGS so things can compile
export ARCHFLAGS="-arch x86_64"
# it can find libs
export LIBRARY_PATH=/usr/local/lib/:$LIBRARY_PATH
export CPATH=/usr/local/include/:$CPATH
export DYLD_LIBRARY_PATH=/usr/local/opt/openssl/lib:$DYLD_LIBRARY_PATH

# nvm
export NVM_DIR="$HOME/.nvm"
source "$(brew --prefix nvm)/nvm.sh"
