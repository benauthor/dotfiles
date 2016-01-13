#!/bin/sh

shopt -s dotglob
BRANCH=osx
echo my-dotfiles bootstrap for $BRANCH systems
cd ~

ensure_dir () {
    if [ ! -d $1 ]
    then
        echo Creating directory: $1
        mkdir $1
    else
        echo Directory exists: $1
    fi
}

#setup useful dirs
ensure_dir ~/public_html
ensure_dir ~/local
ensure_dir ~/local/bin

#get my-dotfiles repo
if [ ! -d ~/dotfiles ]
then
    echo 'cloning dotfiles'
    git clone -b $BRANCH git@github.com:benauthor/dotfiles.git
fi

# get virtualenv
#if [ ! -f ~/local/bin/virtualenv.py ]
#then
#    wget https://raw.github.com/pypa/virtualenv/master/virtualenv.py
#    mv virtualenv.py ~/local/bin/virtualenv.py
#fi

# get powerline
if [ ! -d ~/local/powerline-shell ]
then
    echo 'cloning powerline-shell'
    git clone https://github.com/milkbikis/powerline-shell ~/local/powerline-shell
    ~/local/powerline-shell/install.py
fi

dotfiles=( vimrc bashrc screenrc emacs profile bash_aliases tmux.conf gitignore )
for file in ${dotfiles[@]}
do
    dotted=.$file
    if [ -f $dotted ]
    then
        echo Backing up $dotted to $file.old
        mv $dotted $file.old
    fi
    echo linking ~/dotfiles/$file to $dotted
    ln -s ~/dotfiles/$file $dotted
done
