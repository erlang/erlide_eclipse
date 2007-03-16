set ECLIPSE_HOME=%1
set ERLANG_HOME=%2
set WORKSPACE=%3

java -jar %ECLIPSE_HOME%/startup.jar -application org.eclipse.ant.core.antRunner -Dbuilder=%WORKSPACE%/org.erlide.build -DlocalWorkspace=%WORKSPACE% -DerlRoot=%ERLANG_HOME% %4 %5 %6 %7 %8 %9


