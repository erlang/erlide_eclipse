#! /bin/bash

BASE=$(dirname $0)
ROOT=$1
VER=$2

if [ -z "$VER" ]; then
    echo "Insufficient parameters: need $root and $version"
    exit 1
fi

PROJECTS=`find $ROOT/features $ROOT/plugins $ROOT/tests -name "pom.xml" | xargs dirname | xargs basename -a | grep org.erlide | paste -s -d,`

pushd $BASE/.. > /dev/null

./mvnw tycho-versions:set-version -Dtycho.mode=maven -DnewVersion=$VER -Dartifacts=$PROJECTS

popd > /dev/null
