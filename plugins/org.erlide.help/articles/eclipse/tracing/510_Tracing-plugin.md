---
layout: article_eclipse
part: Tracing plugin
---

# Tracing plugin

> _In software engineering, tracing is a specialized use of logging to record information about a program's execution. This information is typically used by programmers for debugging purposes, and additionally, depending on the type and detail of information contained in a trace log, by experienced system administrators or technical support personnel to diagnose common problems with software_.  
[Wikipedia](https://en.wikipedia.org/wiki/Tracing_(software\))

## Features
* New eclipse perspective and views
* Setting process flags
* Tracing multiple nodes at the same time (also nodes started outside erlide)
* Setting net nick time and cookie per node
* Selecting processes which should be traced
* Setting trace patterns for functions (support for match specifications)
* New positions in outline's context menu for creating trace patterns from selected function
* Saving and restoring trace patterns configuration
* Saving and restoring nodes configuration
* Browsing trace results in tree
* Loading trace results from disk

## Technical details

### Ttb
This feature uses ttb (Trace Tool Builder, https://www.erlang.org/doc/man/ttb.html ) for tracing purposes. 
Ttb is a tool included in OTP which is is built on top of dbg. Tracing plugin provides graphical user interface to ttb what makes using it more user friendly. 
Ttb module is currently included in this plugin. That's because original ttb module had some bugs. Module used by this plugin contains many improvements and fixes which haven't been yet included into OTP (ttb version which is used by this plugin has been already sent to OTP team so it should be included in future release).

### Tracing
Ttb is started on separate erlang node called "tracing" ( `-sname tracing` , if necessary this name can be changed from preferences window ). It allows tracing tool to be independent from nodes you want to trace. "Tracing" node is automatically started by erlide when any tracing action is performed for the first time (tracing another node or loading trace data from file).
Traced nodes are treated as "diskless nodes", i.e. all data from them is directly sent to the "tracing" node. After finishing tracing all data is writen to the directory named _ttb_upload_erlide_tracing-Timestamp_ ( _Timestamp_ is in a form of _yyyymmdd-hhmmss_) into file called _nodeName@hostName_ (there will be one file per node).

When tracing is finished application reads all data from this directory using special data handler provided to `ttbe:format/2` function which, instead of formatting data, sends it directly to trace plugin which interprets it.
Same action is performed when reading trace results from file.

### Connection
It is possible to connect to multiple nodes in the same time (also nodes started outside erlide) to trace them. However, before starting tracing you should set properly value of _Net Ticktime_ and _cookies_.

_Net Ticktime_

It is very important that all nodes (traced nodes and tracing one) have set the same value of _Net Ticktime_ because this value is used for detecting nodes that are not responding.
Once every `TickTime /4` seconds all connected nodes are "ticked". Time in which not responding node is detected is `0.75 * TickTime < T < 1.25 * TickTime` .
_Net Ticktime_ is set using `net_kernel:set_net_ticktime/1` function. It's value can be specified in preferences window.

_Cookies_

Every two Erlang nodes communicating with each other should have set the same magic cookie . When you start Erlang node from console you can specify cookie's value using `-setcookie Cookie` or later calling `erlang:set_cookie(node(), Cookie)` .
Each cookie specified in nodes tab is set using `erlang:set_cookie(Node, Cookie)`.

## Implementation details
If you want to understand source code of this plugin this [site](530_Tracing-plugin-implementation.html) might be very useful.

## Examples
[Here](520_Tracing-plugin-example.html) you can find example how to use this plugin.
