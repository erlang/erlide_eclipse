SET eclipseDir=c:\users\vlad\applications\eclipse34_clean\eclipse

SET eclipseLauncher=%eclipseDir%\plugins\org.eclipse.equinox.launcher_1.0.101.R34x_v20081125.jar

"c:\program files\java\jdk1.5.0_14\bin\java" -jar %eclipseLAuncher% -application org.eclipse.ant.core.antRunner -buildfile build-erlide.xml 2>&1 1>build.log

