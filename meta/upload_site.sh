#! /bin/sh

rsync -avP -e ssh erlide_*.zip {$1},erlide@frs.sourceforge.net:/home/frs/project/e/er/erlide/
