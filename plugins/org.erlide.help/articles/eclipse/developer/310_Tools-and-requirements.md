---
layout: article_eclipse
part: Developer's guide
title: Tools and requirements
---

# Tools and requirements

## Prerequisites

* The issue manager is at <https://github.com/erlang/erlide_eclipse/issues>.
* It would be highly recommended to have some background with Java and Eclipse development, the learning curve can be quite steep. You probably won't start by implementing a new plug-in from scratch, but it helps to know how things are working and where to look. Some suggestions for tutorials/articles/books:
  * the eclipse documentation, of course
  * <https://www.vogella.com/articles/EclipsePlugIn/article.html>
  * <https://www.amazon.com/Eclipse-Plug-ins-Edition-Eric-Clayberg/dp/0321553462>

## Tools

* Download and install Eclipse 4.4 or later, which will serve as a target for development.
* You can use Eclipse 4.4 for development too, but the newer Java tools are much better. I always the latest Eclipse for developing.
* The debugger support code has different projects for each supported OTP version. When building erlide the tool `kerl` will be downloaded and install required OTP versions to `~/erlide_tools/`.
* In your development Eclipse, install a Java 1.8 JDK and the _EGit_, _Erlide_ and _Xtend_ plugins.
* Other recommended plugins: AnyEdit.
* A note about using more recent versions of Java/Eclipse/Erlang: they should work, but if the target is not 1.8/4.4/23, then you might use things that are not available in the base configuration or have changed since and then there will be errors (at compile-time or run-time).

## Setup

* Start Eclipse and
  * configure the Plugin development environment by setting the target environment to the 4.4 installation
  * configure Erlide with runtimes: one of all supported versions (because we need the debugger to get compiled with each of them).
  * restart
* In your Eclipse workspace, import all projects from the erlide repository, even the optional ones (wrangler, cover, tracing). This will let you know if you happen to change something that affects them.
* Close the wrangler.refactoring.codeinspection plugin, it only compiles on the build server. (Alternatively, leave it open and live with a couple of compilation errors).
* Configure some settings by importing the preferences from `meta/config`, this will help to keep a consistent view of the code.

* The projects should build without errors.

* The "Run" button's (green with white arrow) popdown menu should contain a few entries: some junit_* ones that run the unit tests for the respective project and "Erlide_tests" that launches a new Eclipse instance that uses the local erlide code (including any changes you made). You can import in there all projects from `org.erlide.ui.tests/projects`
