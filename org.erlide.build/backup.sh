#!/bin/bash

STAMP=$(date +%Y%m%d)
cd erlide-svnroot
rsync -av erlide.svn.sourceforge.net::svn/erlide/* .
tar -cf ../erlide-svnroot.${STAMP}.tar .
cd ..
gzip -f erlide-svnroot.${STAMP}.tar


#
#  Only keep the most recent NUMLOGS archives
#

NUMLOGS=4
#
#
for i in `find . -maxdepth 1 -name "erlide-svnroot*gz"  -printf "%A@ %p\n" 2> /dev/null | sort |awk 'NR > '"${NUMLOGS}"' {print $2}'`
do
        echo "old: $i";
        #rm $i
done
