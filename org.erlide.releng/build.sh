#!/bin/bash -xe

export TOOLS=${1:-"${HOME}/erlide_tools"}

export PATH=${TOOLS}/bin:$PATH
export ANT_HOME=${TOOLS}/ant

VER_=$(cat ${WORKSPACE}/org.erlide/feature.xml | grep "version=.*qualifier" | head -n 1 | cut -d\" -f 2 | cut -d. -f1,2,3)
echo "document.write('$(git describe)');" > info
echo "document.write('$VER_');" > version
echo "document.write('$BUILD_ID');" > id

ant -f org.erlide.releng/build.ant build

