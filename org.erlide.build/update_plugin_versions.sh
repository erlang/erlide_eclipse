#! /bin/sh

BASE=$1

CRT=$(git branch | grep '*' | cut -d ' ' -f 2)

PROJECTS=$(git log --name-only $1..$CRT --oneline | cut -d ' ' -f 1 | grep org.erlide | cut -f 1 -d '/' | sort | uniq)

VSNS=""
git checkout -q $BASE
if [ $? -ne 0 ]
then
  echo "Can only run in a clean branch, you have uncommited modifications."
  exit 1
fi

for PRJ in $PROJECTS
do
  if [ -a $PRJ/META-INF/MANIFEST.MF ] 
  then 
    OLD=$(cat $PRJ/META-INF/MANIFEST.MF | grep Bundle-Version: | cut -d ' ' -f 2)
    VSNS="$PRJ:$OLD $VSNS"
  fi
done
git checkout -q $CRT
RESULT=""
for VSN in $VSNS
do
  PRJ=$(echo $VSN | cut -d ':' -f 1)
  OLD=$(echo $VSN | cut -d ':' -f 2)
  if [ -a $PRJ/META-INF/MANIFEST.MF ] 
  then  
    NEW=$(cat $PRJ/META-INF/MANIFEST.MF | grep Bundle-Version: | cut -d ' ' -f 2)
	if [ $OLD = $NEW ]
	then
	  RESULT="$VSN $RESULT"
	fi
  fi
done

for VSN in $RESULT
do
  PRJ=$(echo $VSN | cut -d ':' -f 1)
  OLD=$(echo $VSN | cut -d ':' -f 2)

  MAJ=$(echo $OLD | cut -d '.' -f 1)
  MIN=$(echo $OLD | cut -d '.' -f 2)
  MICRO=$(echo $OLD | cut -d '.' -f 3)
  QUAL=$(echo $OLD | cut -d '.' -f 4)
  MICRO2=$(($MICRO + 1))
  if [ -z $QUAL ] 
  then
    NEW="$MAJ.$MIN.$MICRO2"
  else
    NEW="$MAJ.$MIN.$MICRO2.$QUAL"
  fi
  echo "$PRJ:: $OLD -> $NEW"
  sed "s/Bundle-Version: $OLD/Bundle-Version: $NEW/" < $PRJ/META-INF/MANIFEST.MF > $PRJ/META-INF/MANIFEST.MF1
  mv $PRJ/META-INF/MANIFEST.MF1 $PRJ/META-INF/MANIFEST.MF
done

#git commit -a -m "updated version numbers"



