#!/bin/bash

# install the required things for building erlide
#
# run as user who will run the builds (jenkins)
#
# script must be run from user's home
# the tools are installed in erlide_tools

ROOT_DIR=$1

if [ -z "$ROOT_DIR" ]
  then
    echo "Specify root dir as argument!"
    exit 1
fi

mkdir -p $ROOT_DIR/erlide_tools
pushd $ROOT_DIR/erlide_tools

# jdk 1.6
if [ ! -d jdk ] ; then
  if [ ! -f jdk-6u45-linux-x64.bin ] ; then
      wget http://download.erlide.org/tools/jdk-6u45-linux-x64.bin
  fi
  chmod u+x jdk-6u45-linux-x64.bin
  ./jdk-6u45-linux-x64.bin
  ln -s jdk1.6.0_45 jdk
  rm jdk-6u45-linux-x64.bin
fi

# ant 1.9.2
if [ ! -d ant ] ; then
  if [ ! -f apache-ant-1.9.2-bin.tar.gz ] ; then
    wget http://download.erlide.org/tools/apache-ant-1.9.2-bin.tar.gz
  fi
  tar zxvf apache-ant-1.9.2-bin.tar.gz
  ln -s apache-ant-1.9.2 ant
  rm apache-ant-1.9.2-bin.tar.gz
fi

# jruby 1.7
if [ ! -d jruby ] ; then
  if [ ! -f jruby-bin-1.7.12.tar.gz ] ; then
    wget http://download.erlide.org/tools/jruby-bin-1.7.12.tar.gz
  fi
  tar zxvf jruby-bin-1.7.12.tar.gz
  ln -s jruby-1.7.12 jruby
  rm jruby-bin-1.7.12.tar.gz

  jruby/bin/jruby -S gem install rake
fi

# these are supposed to be installed (jenkins user is not a sudoer anyway)
# sudo apt-get update
# sudo apt-get install -y build-essential make perl gcc sed m4 libncurses5-dev ncurses-dev libssl-dev autoconf

# erlang R15
if [ ! -d otp15 ] ; then
  if [ ! -f otp_src_R15B03-1.tar.gz ] ; then
    wget http://download.erlide.org/tools/otp_src_R15B03-1.tar.gz
  fi
  tar zxvf otp_src_R15B03-1.tar.gz
  rm otp_src_R15B03-1.tar.gz

  pushd otp_src_R15B03
  export LANG=C
  ./configure
  make
  popd

  ln -s otp_src_R15B03 otp15
fi

if [ ! -d otp16 ] ; then
  if [ ! -f otp_src_R16B03.tar.gz ] ; then
    wget http://download.erlide.org/tools/otp_src_R16B03.tar.gz
  fi
  tar zxvf otp_src_R16B03.tar.gz
  rm otp_src_R16B03.tar.gz

  pushd otp_src_R16B03
  export LANG=C
  ./configure
  make
  popd

  ln -s otp_src_R16B03 otp16
fi

if [ ! -d otp17 ] ; then
  if [ ! -f otp_src_17.0.tar.gz ] ; then
    wget http://download.erlide.org/tools/otp_src_17.0.tar.gz
  fi
  tar zxvf otp_src_17.0.tar.gz
  rm otp_src_17.0.tar.gz

  # build erlang
  pushd otp_src_17.0
  export LANG=C
  ./configure
  make
  popd

  ln -s otp_src_17.0 otp17
fi

ln -s otp15 otp

# TODO install eclipse target

echo "add links to path"
export PATH=`pwd`/ant/bin:`pwd`/jruby/bin:$PATH
echo "export PATH=$PATH" >> $ROOT_DIR/.bashrc

popd
