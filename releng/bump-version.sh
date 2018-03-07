#! /bin/sh

BASE=$(dirname $0)
ROOT=$1
BUMP=$2 # major|minor|micro
#TODO check arguments

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
  echo "$NEW"
}


pushd $BASE/.. > /dev/null

FEATURE=$(ls $ROOT/features | head -1)
CRTVER=$(cat $ROOT/features/$FEATURE/feature.xml | grep -v "?xml" | grep "version=" | head -1 | tr -d '[:space:]')
prefix='version="'
suffix='"'
CRTVER=${CRTVER#$prefix}
CRTVER=${CRTVER%$suffix}

NEWVER=$(inc_version $CRTVER $BUMP)

releng/set-versions.sh $ROOT $NEWVER

popd > /dev/null
