---
layout: article_eclipse
title: Installing and updating
part: Getting started
---

# Installing and updating

* Install Erlang __R15__ or later, if it isn't already present on your system. On Windows systems, use a path with no spaces in it.
* Install Eclipse. We target primarily version __3.8/4.2__, later versions work just as well.
* If your network uses a proxy to connect to the internet, fill in the appropriate data in `Window -> Preferences -> Install/Update -> Proxy settings`
* Install Erlide by going to `Help -> Software Updates -> Find` and `Install... -> Search for new features to install`. 
* In the dialog, choose New remote site and enter `Erlide` as name and `http://download.erlide.org/update` as URL.
* Select `Erlang IDE` and maybe the optional add-ins. Press `Next`, again `Next`, accept the license agreement and `Finish`. You may be asked to agree to install unsigned content, do so.
* Restart. Go to `Window -> Preferences -> Erlang -> Installed runtimes` and add an entry (or several) for your Erlang installation(s) of choice. The required parameters are the name and the path to the top level directory (i.e. `$ERL_TOP`). Now restart again.
* Done! You're ready to start exploring.

## Update sites

* Stable releases: [http://download.erlide.org/update](http://download.erlide.org/update)
* Beta releases: [http://download.erlide.org/update/beta](http://download.erlide.org/update/beta)
* Nightly releases (bleeding edge, may not work, use at your own risk): [http://download.erlide.org/update/nightly](http://download.erlide.org/update/nightly)
