#!/bin/bash -xe

export TOOLS=${1:-"${HOME}/erlide_tools"}

export PATH=${TOOLS}/bin:$PATH
export ANT_HOME=${TOOLS}/ant

mkdir ${WORKSPACE}/buildroot

VER_=$(cat ${WORKSPACE}/org.erlide/feature.xml | grep "version=.*qualifier" | head -n 1 | cut -d\" -f 2 | cut -d. -f1,2,3)
echo "document.write('$(git describe)');" > ${WORKSPACE}/buildroot/info.js
echo "document.write('$VER_');" > ${WORKSPACE}/buildroot/version.js
echo "document.write('$BUILD_ID');" > ${WORKSPACE}/buildroot/id.js

ant -f org.erlide.releng/build.ant run_tests

