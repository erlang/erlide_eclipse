#! /bin/sh
scp -r features/ ${1},erlide@frs.sourceforge.net:/home/frs/project/e/er/erlide/update
scp -r plugins/ ${1},erlide@frs.sourceforge.net:/home/frs/project/e/er/erlide/update
scp web/site* ${1},erlide@frs.sourceforge.net:/home/frs/project/e/er/erlide/update/web
scp index.html ${1},erlide@frs.sourceforge.net:/home/frs/project/e/er/erlide/update
scp site.xml ${1},erlide@frs.sourceforge.net:/home/frs/project/e/er/erlide/update
