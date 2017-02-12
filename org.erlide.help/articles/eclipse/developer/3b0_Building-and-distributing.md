---
layout: article_eclipse
part: Developer's guide
---
# Building and distributing

## Update sites

There are three update sites:

-   `http://download.erlide.org/update` : contains all stable releases
-   `http://download.erlide.org/update/beta` : contains the release the will be
    promoted to stable once it passes acceptance tests. May be broken at
    times, but not for long.
-   `http://download.erlide.org/update/nightly` : contains the latest and
    greatest, but untested code. Use it at your own risk.

## Building Erlide

Building is done using [Buckminster](http://eclipse.org/buckminster).
This has the great advantage that the build is done inside a regular
Eclipse workspace, with the regular builders for that project, and we
don’t have to bother to add custom build scripts for Erlang code (that
also has to use the same config as in the IDE).

For headless builds, the `org.erlide.releng` project contains all the
necessary scripts and more detailed information. To drive the builds,
there are is a `Rakefile` script (so you need to have jruby and rake
installed) which more or less calls Ant with `build.ant`.

The Ant script can install Buckminster and there is also a
`setup_tools.sh` script that will install java, ant, otp R15 and jruby
in your home directory. (JRuby is used because it had seamless integration with Ant)

For building the update site from inside Eclipse, right-click on the
`org.erlide.site` project and choose `Buckminster&rarr;Invoke action`. In the
dialog, specify org.erlide.site/buckminster.properties as properties
file and select `site.p2` as action. 

### Build target

The target is installed automatically by the scripts, if it’s not
already there. It will be put in `~/erlide_tools/target.platform` so
that it can be reused.

### Automatic builds

We have a [Jenkins](http://jenkins-ci.org) server set up to do automatic
builds, at `http://ci.erlide.org`. Here we will run automated tests and
stable releases can be published to the official update sites from here
too.

#### Jenkins jobs

There are a number of jobs that roughly correspond to the different
build tasks.

-   **build_pu**, **build_beta**, **build_master**: these are running the test suite on
    the respective branches from the repo (pu, release, master)
-   **p2\_site**: if the tests are successful, a p2 update site is
    created
-   **publish\_site**: picks a p2 update site and makes it available on
    the web. `pu` is published automatically to `nightly`, while `beta`
    and `master` are published manually by promoting the respective
    builds
-   **publish\_product**: puts together an Eclipse-based erlide product
    and publishes it. Only from `master` and is manually started after
    acceptance tests. 

