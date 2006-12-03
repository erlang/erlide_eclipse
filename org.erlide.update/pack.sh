#! /bin/sh

VER=$(ls features/org.erlide_* | sed -e 's&features/org.erlide_&&' | sed -e 's/.jar//')
echo VER=${VER}@


rm -rf temp
mkdir temp

cp -r features/ temp/
cp -r plugins/ temp/
cp web/site* temp/
cp index.html temp/
cp site.xml temp/

rm erlide_updatesite_*
cd temp
zip -r ../erlide_updatesite_${VER}.zip *
cd ..

mv erlide.zip erlide_${VER}.zip

