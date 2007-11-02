steps to setup a build server:

1. Create a new folder for the build (e.g. ~/erlide/releng) and CD to this directory.
2. Fetch the eclipse base builder from eclipse CVS repository

Fetch the eclipse base builder using the following command
# cvs -d :pserver:anonymous@dev.eclipse.org:/cvsroot/eclipse export -r r322_v20070104  org.eclipse.releng.basebuilder 

3. Download the subversion pde build plugin from the sourceforge.net svn-pde-build project

Download the svn-pde-build zip file from the sourceforge web site, 
https://sourceforge.net/projects/svn-pde-build/

Extract the svn-pde-build zip file over the org.eclipse.releng.basebuilder directory.

4. Fetch the erlide builder from  
https://sourceforge.net/projects/erlide/

[provide compiled archive!!]

5. Start build

CD to the builder directory created by the svn export and start the build with the following command.

# java -cp ..\org.eclipse.releng.basebuilder\startup.jar org.eclipse.core.launcher.Main -application org.eclipse.ant.core.antRunner -buildfile build.xml -Dcomponent=org.erlide.build -DbuildDirectory=~/erlide/releng -Dbase=~/erlide

The results of the build will be in the directory ~/erlide/releng/I<timestamp>





$ java -cp ../org.eclipse.releng.basebuilder/startup.jar org.eclipse.core.launcher.Main -application org.eclipse.ant.core.antRunner -buildfile build.
xml -Dcomponent=erlide -DbaseLocation=c:/programs/eclipse -Dbase=c:/programs/erlide32/erlide.release/base -DbuildDirectory=c:/programs/erlide32/erlid
e.release/temp


