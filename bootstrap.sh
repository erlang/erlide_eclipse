#!/bin/bash

sudo apt-get update
sudo apt-mark hold grub2 grub2-common grub-pc grub-pc-bin
sudo apt-get upgrade -y
sudo apt-get install -y erlang ant openjdk-6-jdk git ruby1.9.3 jruby
sudo apt-get autoremove

rm -rf $HOME/erlide
ln -s /vagrant $HOME/erlide

