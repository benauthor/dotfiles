# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# globstar, requires bash 4
shopt -s globstar

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=5000
HISTFILESIZE=10000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
   #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# git completion
source /Library/Developer/CommandLineTools/usr/share/git-core/git-completion.bash

# pretty prompt
source /Library/Developer/CommandLineTools/usr/share/git-core/git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=1

__prompt_command() {
    local EXIT="$?"             # This needs to be first
    PS1=""

    local RCol='\[\e[0m\]'

    local Red='\[\e[0;31m\]'
    local Blu='\[\e[0;34m\]'
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

# eh junk
source ~/.aws/profile
export DEVENV_MEMORY=8000
export DEVENV_CPUS=3
export EH_PLAYBOOKS_ROOT=~/work/playbooks
source ~/work/playbooks/includes/utility_functions.sh


export JAVA_8_HOME=$(/usr/libexec/java_home -v1.8)
export JAVA_9_HOME=$(/usr/libexec/java_home -v1.9 2> /dev/null)

alias java8='export JAVA_HOME=$JAVA_8_HOME'
alias java9='export JAVA_HOME=$JAVA_9_HOME'

#default java8
export JAVA_HOME=$JAVA_8_HOME

export ANDROID_HOME=/usr/local/share/android-sdk

# our version of gpg doesn't start a new agent every
# time, so this easy way is safe
eval $( gpg-agent --daemon 2>/dev/null )

# added by travis gem
[ -f /Users/bender/.travis/travis.sh ] && source /Users/bender/.travis/travis.sh

# tex
# export PATH=$PATH:/Library/TeX/texbin
export PATH=$PATH:~/local/bin

# rbenv
export PATH=~/.rbenv/shims:$PATH

# nvm
export NVM_DIR="$HOME/.nvm"
# shellcheck disable=SC1091
. "/usr/local/opt/nvm/nvm.sh"

# go
export GOPATH=~/go

# local tomcat
export PATH=/usr/local/opt/tomcat@7/bin:$PATH
