#! /bin/sh

rsync -avP -e ssh erlide_*.zip vladdu@frs.sourceforge.net:uploads/
