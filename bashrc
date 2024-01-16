# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# shush mac zsh warning
export BASH_SILENCE_DEPRECATION_WARNING=1

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth
# append to the history file, don't overwrite it
shopt -s histappend
# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=5000
HISTFILESIZE=10000

# globstar, requires bash 4
# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
# shopt -s globstar

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

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

#export JAVA_8_HOME=$(/usr/libexec/java_home -v1.8)
#export JAVA_9_HOME=$(/usr/libexec/java_home -v1.9 2> /dev/null)

#alias java8='export JAVA_HOME=$JAVA_8_HOME'
#alias java9='export JAVA_HOME=$JAVA_9_HOME'

#default java8
#export JAVA_HOME=$JAVA_9_HOME

# our version of gpg doesn't start a new agent every
# time, so this easy way is safe
eval $( gpg-agent --daemon 2>/dev/null )

# added by travis gem
[ -f /Users/bender/.travis/travis.sh ] && source /Users/bender/.travis/travis.sh

# misc local utils
export PATH=~/local/bin:$PATH

# find brew installed dependencies
# XXX this is a mishmash of advice from a couple different sources
# export LD_LIBRARY_PATH="/opt/homebrew/opt/openssl@3/lib"
# export LDFLAGS="-L/opt/homebrew/opt/openssl@3/lib -L/opt/homebrew/lib"
# export CPPFLAGS="-I/opt/homebrew/opt/openssl@3/include -I/opt/homebrew/opt/gperftools/include -I/opt/homebrew/Cellar/rocksdb@6.20.3/6.20.3/include -I/opt/homebrew/Cellar/foundationdb-headers@6.2.30/6.2.30/include"

# export CPATH="/opt/homebrew/include:$CPATH"
# export LIBRARY_PATH="/opt/homebrew/lib:$LIBRARY_PATH"
export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:/opt/homebrew/lib/pkgconfig:/opt/homebrew/Cellar/rocksdb@6.20.3/6.20.3/lib/pkgconfig:/opt/homebrew/opt/openssl/lib/pkgconfig"


# go
export GOPATH=~/go
export PATH=~/go/bin:$PATH
export CGO_CXXFLAGS_ALLOW='-lpthread|-ltcmalloc'
export CGO_CFLAGS_ALLOW='-ltcmalloc'
export CGO_CPPFLAGS="-I/opt/homebrew/opt/openssl@3/include -I/opt/homebrew/opt/gperftools/include -I/opt/homebrew/Cellar/rocksdb@6.20.3/6.20.3/include -I/opt/homebrew/Cellar/foundationdb-headers@6.2.30/6.2.30/include -DCMAKE_EXE_LINKER_FLAG"


source ~/.gimme/envs/go1.21.1.env

# sweet inline plotting
export ITERMPLOT=rv
export MPLBACKEND="module://itermplot"

# include .bashrc_hidden if it exists
# if i.e. I don't want to version control some secrets
if [ -f ~/.bashrc_hidden ]; then
    . ~/.bashrc_hidden
fi

# allow ctl-s for forward history
# because I don't use flow control
stty -ixon

# running out of file handles???
ulimit -S -n $((10240 * 2))


export SSH_ENV="${HOME}/.ssh/environment"

start_ssh_agent() {
    echo "Initialising new SSH agent..."
    ssh-agent -s | sed 's/^echo/#echo/' > "${SSH_ENV}"
    echo succeeded
    chmod 600 "${SSH_ENV}"
    . "${SSH_ENV}" > /dev/null
    ssh-add -k;
}

# Source SSH settings, if applicable
load_ssh_session() {
    if [ -f "${SSH_ENV}" ]; then
        . "${SSH_ENV}" > /dev/null
        ps aux ${SSH_AGENT_PID} | grep 'ssh-agent -s$' > /dev/null || {
            start_ssh_agent;
        }
    else
        start_ssh_agent;
    fi
}

# load_ssh_session

. "$HOME/.cargo/env"

# put various bins on path
export PATH="/opt/homebrew/opt/make/libexec/gnubin:$PATH"
export PATH="/Users/evan.bender/bin:$PATH"
export PATH="/Users/evan.bender/dotfiles/bin:$PATH"
export PATH="/Users/evan.bender/.krew/bin:$PATH"
