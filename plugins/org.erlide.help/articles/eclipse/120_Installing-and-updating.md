---
layout: article_eclipse
title: Installing and updating
part: Getting started
---

# Installing and updating

* Install Erlang __R20__ or later, if it isn't already present on your system. On Windows systems, use a path with no spaces in it.
* Install Eclipse. We target primarily version __4.8__, later versions work just as well. __Java 8__ is required.
* If your network uses a proxy to connect to the internet, fill in the appropriate data in `Window -> Preferences -> Install/Update -> Proxy settings`
* Install Erlide by going to `Help -> Software Updates -> Find` and `Install... -> Search for new features to install`.
* In the dialog, choose New remote site and enter `Erlide` as name and `https://download.erlide.org/update` as URL.
* Select `Erlang IDE` and maybe the optional add-ins. Press `Next`, again `Next`, accept the license agreement and `Finish`. You may be asked to agree to install unsigned content, do so.
* Restart. Go to `Window -> Preferences -> Erlang -> Installed runtimes` and add an entry (or several) for your Erlang installation(s) of choice. The required parameters are the name and the path to the top level directory (i.e. `$ERL_TOP`). Now restart again.
* Done! You're ready to start exploring.

## Update sites

* Stable releases: [https://download.erlide.org/update](https://download.erlide.org/update)
* Beta releases: [https://download.erlide.org/update/beta](https://download.erlide.org/update/beta)
* Nightly releases (bleeding edge, may not work, use at your own risk): [https://download.erlide.org/update/nightly](https://download.erlide.org/update/nightly)
