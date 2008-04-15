#! /bin/sh

java -jar c:/progra~1/eclipse33/plugins/org.eclipse.equinox.launcher_1.0.1.R33x_v20080118.jar \
  -application org.eclipse.update.core.siteOptimizer -digestBuilder \
  -digestOutputDir=digest -siteXML=site.xml  -jarProcessor -processAll -repack -pack \
  -outputDir . .