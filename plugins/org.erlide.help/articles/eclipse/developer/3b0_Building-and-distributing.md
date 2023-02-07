---
layout: article_eclipse
part: Developer's guide
---
# Building and distributing

## Update sites

There are two update sites:

-   `https://erlide.org/update` : contains all stable releases
-   `https://erlide.org/update/prerelease` : contains the release that will be
    promoted to stable once it passes acceptance tests. May be broken at
    times, but not for long.

## Building Erlide

Since Erlide consists of two separate parts, in two separate repos, we need to build and release in two steps:

### 1. Build erlide_kernel

The Erlang part of the plugin is located in the [erlide_kernel]( https://github.com/erlang/erlide_kernel) repo.

First build and generate a plugin archive for erlide_kernel:

```
cd <path to erlide_kernel repo>

# Build and test the Erlang modules
./build
./build test

# Generate the plugin archive
cd eclipse
./build
```

The result can be found at:
`<repo root>/eclipse/org.erlide.kernel.site-X.XXX.X.zip`

#### Publish

We use the project update site to also publish the build dependencies.
The project update site `https://erlide.org/update` is deployed by the [erlide.github.io](https://github.com/erlide/erlide.github.io) repo.

To publish a new version of erlide_kernel the built archive needs to be extracted
to the [erlide.github.io](https://github.com/erlide/erlide.github.io) repo
at the path `<root>/update/kernel/<VERSION>` like this:

```
git clone git@github.com:erlide/erlide.github.io.git
cd erlide.github.io/update/kernel/
mkdir -p <VERSION>
unzip <erlide_kernel repo>/eclipse/org.erlide.kernel.site-X.XXX.X.zip -d <VERSION>/
git add <VERSION>
git commit -a -m 'Publish erlide_kernel X.XXX.X' && git push origin master
```


### 2. Build erlide_eclipse

The Eclipse GUI part is located in the [erlide_eclipse]( https://github.com/erlang/erlide_eclipse) repo.

#### Update erlide_kernel version

The following files needs to be updated to use a new published `erlide_kernel`:

```
pom.xml
releng/org.erlide.site/category.xml
releng/org.erlide.target/org.erlide.target.target
releng/org.erlide.target/org.erlide.target.tpd
```

Update the version and the feature name.

#### Build and test

The makefile provides a default target to build and run all unit tests:

`make`

This target also creates a zip archive with the plugin (includes dependencies) at:
`releng/org.erlide.site/target/org.erlide-X.XX.X.vXXXXXXXX-XXXX.zip`

which can be used to install the plugin in Eclipse.

#### Publish release

To publish the plugin to the update site run:

`make publish`

This adds a commit to the update site `https://github.com/erlide/erlide.github.io`
which makes the release available from `https://erlide.org/update`.

#### Publish documentation

To publish the documentation from `plugins/org.erlide.help/articles/*` run:

`make publish-docs`

This adds a commit to the repo `https://github.com/erlide/erlide.github.io`
which updates the documentation available at `https://erlide.org/articles/eclipse/`.
