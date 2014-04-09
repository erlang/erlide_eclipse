#!/bin/bash

apt-get update
#apt-mark hold grub2 grub2-common grub-pc grub-pc-bin
apt-get upgrade -y
apt-get install -y build-essential git dos2unix erlang openjdk-6-jdk
apt-get autoremove

echo "Europe/Stockholm" > /etc/timezone
dpkg-reconfigure --frontend noninteractive tzdata

######

rm -rf /home/vagrant/erlide

su - vagrant

cd /home/vagrant
ln -s /vagrant /home/vagrant/erlide

dos2unix erlide/org.erlide.releng/setup_tools.sh
erlide/org.erlide.releng/setup_tools.sh

