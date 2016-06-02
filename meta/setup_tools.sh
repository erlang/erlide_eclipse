#!/bin/bash

# install the required things for building erlide
#
# run as user who will run the builds (jenkins)
#
# the tools are installed in $DEST_DIR

DEST_DIR=$1

if [ -z "$DEST_DIR" ]
  then
    echo "Specify destination dir as argument!"
    exit 1
fi

mkdir -p $DEST_DIR
pushd $DEST_DIR

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

# ruby must be installed

if [ ! -d ~/.rbenv ] ; then
  git clone https://github.com/sstephenson/rbenv.git ~/.rbenv
  echo 'export PATH="$HOME/.rbenv/bin:$PATH"' >> ~/.bash_profile
  echo 'eval "$(rbenv init -)"' >> ~/.bash_profile
  git clone https://github.com/sstephenson/ruby-build.git
  push ruby-build
  ./install.sh
  popd
fi
rbenv install 2.1.2
rbenv global 2.1.2
gem install bundler

# these are supposed to be installed (jenkins user is not a sudoer anyway)
# sudo apt-get update
# sudo apt-get install -y build-essential make perl gcc sed m4 libncurses5-dev ncurses-dev libssl-dev autoconf

curl -O https://raw.githubusercontent.com/spawngrid/kerl/master/kerl
chmod a+x kerl

./kerl build R15B03-1 r15b03-1
./kerl install r15b03-1 R15B03-1
ln -s R15B03-1 otp15

./kerl build R16B03-1 r16b03-1
./kerl install r16b03-1 R16B03-1
ln -s R16B03-1 otp16

./kerl build 17.3 17.3
./kerl install 17.3 17.3
ln -s 17.3 otp17

ln -s otp15 otp

# TODO install eclipse target

#curl -O https://github.com/rebar/rebar/wiki/rebar
git clone git://github.com/vladdu/rebar.git rebar.git
cd rebar.git
git checkout erlide
./bootstrap
cp rebar ..
cd ..

git clone git://github.com/idubrov/covertool.git
cd covertool
make
cd ..

cat >> rebar_k <<EOF
#! /bin/bash

OTP=$1
shift
. ~/erlide_tools/$OTP/activate
ERL_LIBS=~/erlide_tools/:$ERL_LIBS ~/erlide_tools/rebar -vv $@
kerl_deactivate
EOF
chmod u+x rebar_k

echo "add links to path"
export PATH=$DEST_DIR/ant/bin:$PATH
echo "export PATH=$DEST_DIR/ant/bin:\$PATH" >> ~/.bash_profile

popd
