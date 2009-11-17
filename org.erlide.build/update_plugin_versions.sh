#! /bin/sh

BASE=$1
COMMIT=$2

CRT=$(git branch | grep '*' | cut -d ' ' -f 2)
PROJECTS=$(git log --name-only $1..$CRT --oneline | cut -d ' ' -f 1 | grep org.erlide | cut -f 1 -d '/' | sort | uniq)

for PRJ in $PROJECTS
do
  if [ -a $PRJ/META-INF/MANIFEST.MF ] 
  then 
    OLDFILE=$(git show $BASE:$PRJ/META-INF/MANIFEST.MF 2> /dev/null)
	ERR=$?
	if [ $ERR -eq 0 ] 
	then
      OLD=$(git show $BASE:$PRJ/META-INF/MANIFEST.MF | grep Bundle-Version: | cut -d ' ' -f 2)
      NEW=$(cat $PRJ/META-INF/MANIFEST.MF | grep Bundle-Version: | cut -d ' ' -f 2)
	  if [ $OLD = $NEW ]
	  then
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
		  if [ "$COMMIT" != "dry" ]
		  then
		    sed "s/Bundle-Version: $OLD/Bundle-Version: $NEW/" < $PRJ/META-INF/MANIFEST.MF > $PRJ/META-INF/MANIFEST.MF1
		    mv $PRJ/META-INF/MANIFEST.MF1 $PRJ/META-INF/MANIFEST.MF
		  fi
	  fi
	fi
  fi
done

if [ "$COMMIT" = "co" ] 
then
  git commit -a -m "updated version numbers *"
fi




