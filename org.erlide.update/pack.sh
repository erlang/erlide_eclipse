#! /bin/sh

rm -rf temp
mkdir temp

cp -r features/ temp/
cp -r plugins/ temp/
cp web/site* temp/
cp index.html temp/
cp site.xml temp/

rm erlide_updatesite_*
cd temp
zip -r ../erlide_updatesite_${1}.zip * 
#tar -cvf ../erlide_updatesite_${1}.tar *
cd ..
#gzip erlide_updatesite_${1}.tar
