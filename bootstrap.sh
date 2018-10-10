#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"

declare -a files=(emacs vimrc bashrc profile gitignore screenrc bash_aliases gemrc)

for file in "${files[@]}"; do
	echo Bootstrapping $file
	dotname=".$file"
	echo $dotname
	if [ -e ../$dotname ]; then
		echo $dotname exists, moving
		mv ../$dotname ../$dotname.old
	fi
	echo linking $file
	ln -s $DIR/$file ../$dotname
done
