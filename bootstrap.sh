#!/bin/sh

shopt -s dotglob
BRANCH=debian
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
ensure_dir ~/.vim
ensure_dir ~/.vim/bundle

#get my-dotfiles repo
if [ ! -d ~/my-dotfiles ]
then
    echo 'cloning dotfiles'
    git clone -b $BRANCH git@code.usnews.com:bendere/my-dotfiles.git ~/my-dotfiles
fi

#get vundle
if [ ! -d ~/.vim/bundle/vundle ]
then
    echo 'cloning vundle'
    git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
fi

# get virtualenv
#if [ ! -f ~/local/bin/virtualenv.py ]
#then
#    wget https://raw.github.com/pypa/virtualenv/master/virtualenv.py
#    mv virtualenv.py ~/local/bin/virtualenv.py
#fi

#get powerline
if [ ! -d ~/local/powerline-shell ]
then
    echo 'cloning powerline-shell'
    git clone git@code.usnews.com:bendere/powerline-shell.git ~/local/powerline-shell
#    python ~/local/bin/virtualenv.py -ppython2.6 ~/local/powerline-shell/venv 
    #powerline needs python2.6+ with argparse
    virtualenv --setuptools -ppython2.6 ~/local/powerline-shell/venv
    ~/local/powerline-shell/venv/bin/pip install argparse
fi

dotfiles=( vimrc bashrc screenrc emacs profile bash_aliases )
for file in ${dotfiles[@]}
do
    dotted=.$file
    if [ -f $dotted ]
    then
        echo Backing up $dotted to $file.old
        mv $dotted $file.old
    fi
    echo linking ~/mydotfiles/$file to $dotted
    ln -s ~/my-dotfiles/$file $dotted
done

# get dependencies
# node
# jslint
# etc...
