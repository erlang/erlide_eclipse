#!/bin/bash

apt-get update
#apt-mark hold grub2 grub2-common grub-pc grub-pc-bin
apt-get upgrade -y
apt-get install -y build-essential git openjdk-6-jdk 
apt-get autoremove
apt-get clean

echo "Europe/Stockholm" > /etc/timezone
dpkg-reconfigure --frontend noninteractive tzdata

# configure keyboard ?

# if [ -z grep "en_US.UTF-8" ~/.bashrc ]
# 	then
# cat >> ~/.bashrc <<EOF
# 	export LC_ALL=en_US.UTF-8
# 	export LANG=en_US.UTF-8
# 	export LANGUAGE=en_US.UTF-8
# EOF
# fi


# workaround vbox 4.3.10, must be entered manually
# sudo ln -s /opt/VBoxGuestAdditions-4.3.10/lib/VBoxGuestAdditions /usr/lib/VBoxGuestAdditions

######

cd ~
rm -rf erlide

# which branch?
git clone http://github.com/erlide/erlide.git

sh -c erlide/org.erlide.releng/setup_tools.sh

