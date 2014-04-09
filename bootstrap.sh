#!/bin/bash

apt-get update
#apt-mark hold grub2 grub2-common grub-pc grub-pc-bin
apt-get upgrade -y
apt-get install -y build-essential git dos2unix erlang openjdk-6-jdk
apt-get autoremove

echo "Europe/Stockholm" > /etc/timezone
dpkg-reconfigure --frontend noninteractive tzdata

######
## docker

#apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 36A1D7869245C8950F966E92D8576A8BA88D21E9
#sh -c "echo deb http://get.docker.io/ubuntu docker main /etc/apt/sources.list.d/docker.list"
#apt-get install lxc-docker
#sudo groupadd docker
#sudo gpasswd -a vagrant docker
#sudo service docker restart

######

rm -rf /home/vagrant/erlide

su - vagrant

cd /home/vagrant
ln -s /vagrant /home/vagrant/erlide

dos2unix erlide/org.erlide.releng/setup_tools.sh
erlide/org.erlide.releng/setup_tools.sh

chown -R vagrant:vagrant erlide_tools
chown vagrant:vagrant erlide
