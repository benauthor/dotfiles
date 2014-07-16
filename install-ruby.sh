#!/bin/sh

wget http://ftp.ruby-lang.org/pub/ruby/stable-snapshot.tar.gz
tar -xzvf stable-snapshot.tar.gz
cd stable-snapshot
./configure -prefix=$HOME/local
make
make install
