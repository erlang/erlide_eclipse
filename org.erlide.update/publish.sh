#! /bin/sh
scp -r features/ ${1}@shell.sourceforge.net:/home/groups/e/er/erlide/htdocs/update
scp -r plugins/ ${1}@shell.sourceforge.net:/home/groups/e/er/erlide/htdocs/update
scp web/site* ${1}@shell.sourceforge.net:/home/groups/e/er/erlide/htdocs/update/web
scp index.html ${1}@shell.sourceforge.net:/home/groups/e/er/erlide/htdocs/update
scp site.xml ${1}@shell.sourceforge.net:/home/groups/e/er/erlide/htdocs/update
