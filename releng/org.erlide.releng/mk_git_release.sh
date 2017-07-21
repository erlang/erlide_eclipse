#! /bin/bash

VERSION=$1

git checkout master
git merge release --no-ff -m $VERSION
git tag $VERSION -a -m $VERSION

git checkout pu
git merge master --no-ff -m $VERSION

git branch -D release
git push upstream :release

git push upstream master
git push upstream pu
git push upstream pu:next
git push upstream $VERSION

