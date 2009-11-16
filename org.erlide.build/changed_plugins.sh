#! /bin/sh

# prints the list of changed projects since the version specified on the command line

git log --name-only $1..HEAD --oneline | cut -d ' ' -f 1 | grep org.erlide | cut -f 1 -d '/' | sort | uniq
