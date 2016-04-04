#! /bin/bash 

rm -rf buildroot/erlide.github.io
git clone http://github.com/erlide/erlide.github.io.git buildroot/erlide.github.io
pushd buildroot/erlide.github.io 
git checkout master
popd

pushd org.erlide.help
ant -file gen_eclipse_help.xml -Dwebsite=../buildroot/erlide.github.io
popd
