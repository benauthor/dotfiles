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

# our version of gpg doesn't start a new agent every
# time, so this easy way is safe
eval $( gpg-agent --daemon 2>/dev/null )

# misc local utils
export PATH=~/local/bin:$PATH

# C/C++ path junk
export LIBRARY_PATH="/opt/homebrew/lib:$LIBRARY_PATH"
export CPATH="/opt/homebrew/include:$CPATH"


# go
export GOPATH=~/go
export PATH=~/go/bin:$PATH

# source ~/.gimme/envs/go1.26.7.env
export GO111MODULE=auto
export GONOSUMDB=github.com/DataDog,go.ddbuild.io
export GOPRIVATE=
export GOPROXY='https://depot-read-api-go.us1.ddbuild.io/magicmirror/magicmirror/@current/%7Chttps://depot-read-api-go.us1.ddbuild.io/magicmirror/magicmirror/@current/%7Chttps://depot-read-api-go.us1.ddbuild.io/magicmirror/testing/@current/'


# include .bashrc_hidden if it exists
# if i.e. I don't want to version control some secrets
if [ -f ~/.bashrc_hidden ]; then
    . ~/.bashrc_hidden
fi

# allow ctl-s for forward history
# because I don't use flow control
stty -ixon

export SSH_ENV="${HOME}/.ssh/environment"

. "$HOME/.cargo/env"

# put various bins on path
export PATH="/Users/evan.bender/bin:$PATH"
export PATH="/Users/evan.bender/dotfiles/bin:$PATH"
[ -f ~/.config/gitsign/include.sh ] && source ~/.config/gitsign/include.sh

export PATH="$PATH:/Users/evan.bender/.local/bin"

# Pi/mcp-cli user tools
export PATH="$HOME/.pi/agent/bin:$PATH"
