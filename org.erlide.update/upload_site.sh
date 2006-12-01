#! /bin/sh

# you need to put this in your ~/.netrc file
#	machine upload.sf.net login anonymous password erlide@users.sf.net


ftp -i upload.sf.net << MAGIC
binary
passive
cd incoming
mput erlide_*.zip
bye
MAGIC
