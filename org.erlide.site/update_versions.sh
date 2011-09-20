#! /bin/sh

if [ "$#" == "0" ] 
then
	echo "Check which plugin versions need to be updated and add commit info to the CHANGES file"
	echo "Usage: $0 base cmd"
	echo "    base = branch or tag for the latest build"
	echo "    cmd = <nothing> : check which plugins need version updates"
	echo "          run       : modify the plugins versions where needed, do not commit"
	echo "          commit    : as above and commit changes"
fi

BASE=$1
CMD=$2

CRT=$(git branch | grep '*' | cut -d ' ' -f 2)
PROJECTS=$(git log --name-only $BASE..$CRT --oneline | cut -d ' ' -f 1 | grep org.erlide | cut -f 1 -d '/' | sort | uniq)

function inc_version {
  local VER=$1
  local WHICH=$2	

  local MAJ=$(echo $VER | cut -d '.' -f 1)
  local MIN=$(echo $VER | cut -d '.' -f 2)
  local MICRO=$(echo $VER | cut -d '.' -f 3)
  local QUAL=$(echo $VER | cut -d '.' -f 4)
  
  local MAJ2=$MAJ
  local MIN2=$MIN
  local MICRO2=$MICRO
  case "$WHICH" in
    major)
	  MAJ2=$(($MAJ + 1))
	  MIN2=0
	  MICRO2=0
	  ;;
	minor)
	  MAJ2=$MAJ
	  MIN2=$(($MIN + 1))
	  MICRO2=0
	  ;;
	micro)
	  MAJ2=$MAJ
	  MIN2=$MIN
	  MICRO2=$(($MICRO + 1))
	  ;;
	*)
	  ;;
  esac
  
  local NEW=""
  if [ -z $QUAL ] 
  then
	NEW="$MAJ2.$MIN2.$MICRO2"
  else
	NEW="$MAJ2.$MIN2.$MICRO2.$QUAL"
  fi
  echo "      : $VER -> $NEW" >&2 
  echo "$NEW"
}

function which_changed {
  OLD=$1
  NEW=$2

  local MAJ1=$(echo $OLD | cut -d '.' -f 1)
  local MIN1=$(echo $OLD | cut -d '.' -f 2)
  local MICRO1=$(echo $OLD | cut -d '.' -f 3)

  local MAJ2=$(echo $NEW | cut -d '.' -f 1)
  local MIN2=$(echo $NEW | cut -d '.' -f 2)
  local MICRO2=$(echo $NEW | cut -d '.' -f 3)
  
  if [ "$MAJ1" != "$MAJ2" ]
  then
	echo major
  elif [ "$MIN1" != "$MIN2" ] 
  then
    echo minor
  elif [ "$MICRO1" != "$MICRO2" ] 
  then
	echo micro
  else
	echo none
  fi
}

function select_change {
  if [ "$1" = "major" -o "$2" = "major" ]
  then  
    echo major
  elif [ "$1" = "minor" -o "$2" = "minor" ]
  then  
    echo minor
  elif [ "$1" = "micro" -o "$2" = "micro" ]
  then  
    echo micro
  else
    echo "none"
  fi
}

function update_feature {
  FEATURE=$1
  CMD=$2
  
  OLD=$(git show $BASE:$FEATURE/feature.xml | grep "  version=" | head -n 1 | cut -d '"' -f 2)
  NEW=$(cat $FEATURE/feature.xml | grep "  version=" | head -n 1 | cut -d '"' -f 2)
  CH=$(which_changed $OLD $NEW)
  CHG=$(select_change $CH $CHANGED)
  if [ "$CHG" != "$CH" ]
  then
	  VER=$(inc_version $NEW $CHG)
	  echo "-> $VER"
	  
	  if [ "$CMD" != "" ]
	  then
		sed "s/  version=\"$OLD\"/  version=\"$VER\"/" < $FEATURE/feature.xml > $FEATURE/feature.xml1
		mv $FEATURE/feature.xml1 $FEATURE/feature.xml
	  fi

	if [ "$FEATURE" == "org.erlide" ]	  
	then
  		NEW_=$(echo $NEW | sed 's/.qualifier//')
		VER_=$(echo $VER | sed 's/.qualifier//')
		mv CHANGES CHANGES.old		
		echo "List of user visible changes between $NEW_ and $VER_ ($(date +%Y%m%d))" > CHANGES
		echo "" >> CHANGES
		git log v$NEW_..$CRT --oneline >> CHANGES
		echo "" >> CHANGES
		cat CHANGES.old >> CHANGES
		rm CHANGES.old
	fi
  fi
}

CHANGED="none"
for PRJ in $PROJECTS
do
  echo $PRJ
  if [ -a $PRJ/META-INF/MANIFEST.MF ] 
  then 
    OLDFILE=$(git show $BASE:$PRJ/META-INF/MANIFEST.MF 2> /dev/null)
	ERR=$?
	if [ $ERR -eq 0 ] 
	then
      OLD=$(git show $BASE:$PRJ/META-INF/MANIFEST.MF | grep Bundle-Version: | cut -d ' ' -f 2)
      NEW=$(cat $PRJ/META-INF/MANIFEST.MF | grep Bundle-Version: | cut -d ' ' -f 2)
	  echo "    $OLD:$NEW"
	  if [ $OLD = $NEW ]
	  then
		  CH="micro"
                  echo "$PRJ::"
		  NEW=$(inc_version $NEW $CH)
		  
		  if [ "$CMD" != "" ]
		  then
			sed "s/Bundle-Version: $OLD/Bundle-Version: $NEW/" < $PRJ/META-INF/MANIFEST.MF > $PRJ/META-INF/MANIFEST.MF1
			mv $PRJ/META-INF/MANIFEST.MF1 $PRJ/META-INF/MANIFEST.MF
		  fi
	  else
	    CH=$(which_changed $OLD $NEW)
	  fi
	  echo "    changed: $CH"
	  CHANGED=$(select_change $CHANGED $CH)
	fi
  fi
done

echo "final changed: $CHANGED..."
if [ "$CHANGED" != "none" ]
then
	update_feature "org.erlide" $CMD
	update_feature "org.erlide.headless" $CMD
	update_feature "org.erlide.sdk" $CMD
fi

if [ "$CMD" = "commit" ] 
then
  git commit -a -m "prepared $VER_"
fi
