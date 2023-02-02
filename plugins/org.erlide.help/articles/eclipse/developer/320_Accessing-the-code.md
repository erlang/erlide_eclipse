---
layout: article_eclipse
part: Developer's guide
---

# Accessing the code

Well, if you made it to this wiki, you probably know how to fork the code repository and clone it to your PC.

> **Important note:** There is no stable API yet, this is the reason why the version number is still starting with a zero. We are currently working at creating an API, but we prioritize regular development (unless you, the would-be extenders, will start asking for the API)

## Working on the core erlide

The "core erlide" is the basic erlide functionality. There are two eclipse feature projects for it, `org.erlide.headless` and `org.erlide`. The headless part is needed so that we can use erlide to compile Erlang projects in Eclipse without having to start the UI.

### Project descriptions

The projects are split in two categories: some should be independent of Eclipse (util, runtime, model) and the others are not (util_eclipse, backend, core, ui). Currently, the `model` project has Eclipse dependencies, but these will be moved to `core`.

Without Eclipse dependencies:

* `org.erlide.libs`: common third-party libraries with no relation to erlide
* `org.erlide.util`: common utilities
* `org.erlide.runtime`: support for starting, connecting to and calling code from Erlang runtimes
* `org.erlide.kernel.*`: host Erlang code 
* `org.erlide.model`: here is where all the interesting stuff happens. It keeps track of the structure of projects and modules and will provide an API to ask questions about it. At the moment it is implemented in both Java and Erlang and this is what has caused many of our problems so far: the impedance mismatch makes it easy for errors to sneak in. Our plan is to evolve a one-language implementation with an independent API. Ideally we would like to be able to use either Java only (with Xtext) or Erlang only.

With Eclipse dependencies:

* `org.erlide.util_eclipse`: ...
* `org.erlide.backend`: configure and launch Erlang runtimes 
* `org.erlide.core`: adapts the model API to use Eclipse concepts, like resources. Together with ui it will also implement the Eclipse hooks, commands and views necessary to make everything work. Ideally this part will be quite language-independent.
* `org.erlide.ui`: see above

**More info to be added**

## Working on an erlide extension

Most additions will get filed in this category. An extension is a more or less independent piece of functionality that gets integrated in erlide. One good example is Wrangler: it is a standalone tool and the respective plugins make its functionality available for erlide users.

**Info to be added later**
