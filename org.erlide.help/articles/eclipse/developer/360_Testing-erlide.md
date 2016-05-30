---
layout: article_eclipse
part: Developer's guide
---

# Testing erlide

When submitting a patch, if you can’t provide a basic test, please
describe in detail how to test it.

Test frameworks
---------------

-   For Java, we use JUnit. Some related frameworks are evaluated, like
    mockito, hamcrest and jbehave.
-   For Erlang, we use eunit.

There are two kinds of tests:

-   regular JUnit and Erlang tests that don’t require any Eclipse classes. 
    These will run fast.
-   tests that require Eclipse classes. These need to start Eclipse 
    instances, so they will be slower.

Test projects
-------------

There are a number of plugin fragment projects (one for each plugin)
that should contain testing code for the respective plugin.

Headless testing
----------------

We'd like to be able to run all tests headless (i.e. from the command
line and without requiring human intervention). Regular JUnit and Erlang
tests can be run easily in this mode. Tests requiring code Eclipse
classes need support for instantiating a workspace. Those requiring UI
need also a X server to be running (in a server environment Xvfb can be
used to fake it).

See the configuration for the Jenkins jobs for more details.