---
layout: article_eclipse
title: Troubleshooting
part: Getting started
---

# {{page.title}} 

## Backend can't be contacted

There have been many issues lately (summer-fall 2012) caused by the Erlang backend not starting and hanging the whole Eclipse instance. I think that I can finally say that I have a solution that will detect early that something is wrong and notify nicely so that one knows what to do about it. Ultimately, it points to this document, so please let me know if I can improve it.

If you just want to read the solution, [jump down to the next section](#fix).

### The problem

The root cause is that the network settings for your machine might not allow Erlang nodes to function properly. Basically it is because Erlang and Java might use different methods to resolve host names and not always come to the same conclusion.

Some frequent reasons:

* _Your machine's name contain non-ASCII characters_. Please rename it.
* _You run OSX with default hostname, which is `foo.local`_. Erlang strips ".local" and Java doesn't find any host named "foo"
* _For some reason, the host name used by Erlang is not recognized by the OS_. Neither DNS nor /etc/hosts can resolve that name. This is kind of weird because two Erlang nodes can connect, but Java nodes can't see them. This can be tested by running `ping foo` where `foo` is the hostname that Erlang uses: if ping works, then it's our bug, please report it; if not see below.

### What to do about it 
<a name='fix'></a>

The simplest solution is to check the hostname that Erlang assigns to nodes (start `erl -name foo` to see it, or check the Window&rarr; Preferences&rarr; Erlang&rarr; Troubleshooting page) and add it to your /etc/hosts file (or similar) for the right IP address. If you want to debug distributed nodes on several machines, you must not use `127.0.0.1` for this, but the real address on the current network.

For Macs, an alternative solution is to rename your computer from `foo.whatever` to just `foo` (or anyhting without any dots).
