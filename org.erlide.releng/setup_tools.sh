#!/bin/bash

# install the required things for building erlide
#
# run as user who will run (jenkins)
#
# the tools are installed in ~/erlide/tools
#
# jdk 1.6 install requires pressing "enter" once

mkdir -p ~/erlide_tools
pushd ~/erlide_tools

# jdk 1.6
if [ ! -d jdk ] ; then
  if [ ! -f jdk-6u26-linux-i586.bin ] ; then
  	wget http://download.erlide.org/tools/jdk-6u26-linux-i586.bin
  fi
  chmod u+x jdk-6u26-linux-i586.bin
  ./jdk-6u26-linux-i586.bin
  ln -s jdk1.6.0_26 jdk
fi

# ant 1.9.2
if [ ! -d ant ] ; then
  if [ ! -f apache-ant-1.9.2-bin.tar.gz ] ; then
    wget http://download.erlide.org/tools/apache-ant-1.9.2-bin.tar.gz
  fi
  tar zxvf apache-ant-1.9.2-bin.tar.gz
  ln -s apache-ant-1.9.2 ant
fi

# jruby 1.7.4
if [ ! -d jruby ] ; then
  if [ ! -f jruby-bin-1.7.4.tar.gz ] ; then
    wget http://download.erlide.org/tools/jruby-bin-1.7.4.tar.gz
  fi
  tar zxvf jruby-bin-1.7.4.tar.gz
  ln -s jruby-1.7.4 jruby

  jruby/bin/jruby -S gem install rake
fi

# erlang R14
if [ ! -d otp ] ; then
  if [ ! -f otp_src_R14B04.tar.gz ] ; then
    wget http://download.erlide.org/tools/otp_src_R14B04.tar.gz
  fi
  tar zxvf otp_src_R14B04.tar.gz

  # build erlang
  # prerequisites:
  #    sudo apt-get update
  #    sudo apt-get install build-essential make perl gcc sed m4 ncurses-dev

  pushd otp_src_R14B04
  export LANG=C
  ./configure
  make
  popd

  ln -s otp_src_R14B04 otp
fi

# erlang R15
if [ ! -d otp15 ] ; then
  if [ ! -f otp_src_R15B03.tar.gz ] ; then
    wget http://download.erlide.org/tools/otp_src_R15B03.tar.gz
  fi
  tar zxvf otp_src_R15B03.tar.gz

  # build erlang
  # prerequisites:
  #    sudo apt-get update
  #    sudo apt-get install build-essential make perl gcc sed m4 ncurses-dev

  pushd otp_src_R15B03
  export LANG=C
  ./configure
  make
  popd

  ln -s otp_src_R14B04 otp15
fi

echo "add links to path"
mkdir bin
ln -s `pwd`/ant/bin/ant bin/ant
ln -s `pwd`/jruby/bin/jruby bin/jruby

export PATH=`pwd`/bin:$PATH
echo "export PATH=$PATH"

popd
