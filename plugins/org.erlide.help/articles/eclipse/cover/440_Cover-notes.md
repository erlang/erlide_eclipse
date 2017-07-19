---
layout: article_eclipse
part: Cover plugin
---

# Cover notes


Following paragraphs will use eunit plug-in as an example.
The same may apply to any other testing plug-in (for Common Test or QuickCheck),
when those will be implemented.

User may want to perform coverage analysis when running unit tests.
As running the tests is the job of eunit plug-in, eunit plug-in has
to have some means to call Cover.

On the other hand, some users may want to install ErlIDE with eunit but without
cover feature, thus there should be no hard dependency between eunit and cover.

To solve this small plug-in org.erlide.cover.api was created.
It is a part of basic ErlIDE feature and provides the means of checking
if Cover feature is installed and calling its services. If cover feature
is not installed, then UnsupportedOperationException is thrown
when user wants to use it.

Other org.erlide.cover.* plug-ins are part of org.erlide.cover feature
and are thus optional to install.

When author of testing plug-in wants to use Cover plug-in,
they need to:

*   call `org.erlide.cover.api.CoverageAnalysis.isAvailable` method - 
to check if Cover feature is installed
*   get Cover plug-in's backend ( `org.erlide.cover.api.CoverageAnalysis.getBackend`)
*    extend org.erlide.cover.api.AbstractCoverRunner class reimplementing `run` method, and call static methods of org.erlide.cover.api.CoverageAnalysis class   
     
     *    `prepareAnalysis` - before tests. This method recompiles
     test code with coverage support and starts cover node.
     It takes `IConfiguration` instance which specifies which
     module are installed[1] and optional list of additional
     nodes that need to take part in coverage analysis
      *    `performAnalysis` - to be run after tests. It does
     the coverage analysis and updates the UI. Takes no arguments. 

*    call `runCoverageAnalisys` method of Cover plug-in's backend

[1] - one can use org.erlide.cover.api.Configuration class