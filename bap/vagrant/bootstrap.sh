#!/usr/bin/env bash

set -x

INSTALL=/vagrant/INSTALL

apt-get update

START=`awk '/__BEGIN_REQUIRED__/ {print NR + 1; exit 0; }' $INSTALL`
END=$((`awk '/__END_REQUIRED__/ {print NR; exit 0; }' $INSTALL` - $START))

tail -n+$START $INSTALL | head -n$END | bash

echo 0 > /proc/sys/kernel/yama/ptrace_scope

cd /vagrant
./autogen.sh
./configure
make test
