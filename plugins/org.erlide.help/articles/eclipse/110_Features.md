---
layout: article_eclipse
title: Features
part: Getting started
---

# Features

## Wizards

### New Project wizard

Create projects with the `File -> New -> Project -> Erlang project` wizard. If
the project isn't new, you can do an automatic search for source files, by
using the "Discover paths" option. Review the result and add other needed
directories.

### New Module wizard

Create files by right-clicking on the desired directory in the resource view,
and choosing either `File -> New -> Other -> File` and entering a name ending
with erl, or `File -> New -> Erlang -> Module` and following the instructions in
the wizard. Leave empty any fields you don't understand the meaning of.

## Configuration

Preferences related to erlide can be found at `Window -> Preferences -> Erlang`.
Please note that not all options are functional yet.

## Editor

The editor has all the generic Eclipse functionality and adds several Erlang-
specific features:

* Syntax highlighting
* Indentation of Erlang code **Ctrl+I**. The indentation affects the
text selection or the current line only if nothing selected.
* OTP documentation shown for external calls by hovering the cursor over a
function call
* Automatic completion of erlang modules, functions and records:
**Ctrl+Space**. The list with proposals is also shown when entering **:**
(colon).
* Go to declaration of function, macro or record: **F3** when the cursor is
inside that element's name.
* Show declaration of macros and records as a hover window
* Bracket matching: the peer of the bracket at the cursor is highlighted.
* Selective display of functions and declarations (folding)
* context-sensitive menu when right clicking in the editor

## Builder


Building is automatic whenever a file is saved (if `Project -> Build
automatically` is enabled) and the modified module is also reloaded in any
Erlang backend that is linked to that project.

## Outline view

Shows the structure of an Erlang module. The outline view allows navigation in
a module and an overview of the functions in it. Clicking on an item will show
its definition in the editor. The shown functions and declarations can be
filtered and sorted.

In the editor a Quick outline dialog can be used for navigation
(**Ctrl+O**). It has an automatic filtering feature, so you can just begin
writing the function name and the list will only show the elements matching
the prefix.

## Erlang console

The console lets you interact with the Erlang node that is behind the scenes.
At the moment it is not as useful as it may be, as it only connects to the
backend that hosts the Erlide functionality, not the ones where the code under
development is run. This will be fixed in the future.

The console has history (**Ctrl+Up** and **Ctrl+Down**), syntax
highlighting, code completion.

When running/debugging, a "normal" console will show up and it can be used to
interact with the runtime, but it has limited functionality.

## Live expressions view

In this view you can enter expressions that will be reevaluated every time a
file is recompiled or when you press the refresh button in the view. You have
to create a new entry, then go to the expression field and edit it. There is
no need to end the expression with a dot.

> **Warning!** Don't use expressions that might kill or crash the runtime (for example
`init:stop()`), because there are no restrictions yet and you'll get
exactly what you ask for!

## Process list view

A process list similar to etop. Double-clicking a process will show details
about it. At the top, one can choose which backend to show the list from.

## EDoc view

Opening a "Edoc" view (in `Window -> Show view -> Erlang`) will display the
documentation for the function where the cursor is. The documentation is also
shown in a hover window, when the mouse cursor is above a function call.

The OTP documentation is shown, if installed on your system. Also, if the
project contains HTML documentation that is located in an OTP-like location
(i.e. in a `doc/html` directory in the project), then this documentation is
also shown.

## Debugger

We offer all the features of the standard debugger, because we are using it.

* Breakpoints, single-stepping. Breakpoints can be set by double-clicking on the
left-side gutter in the editor.
* Inspection and modification of local variables.
* Distributed debugger, debug on multiple nodes.

## Problems view

The problems view will show all errors and warnings from the compiler.
Clicking them opens the file in the editor at the right line.

There is also a Tasks view that will detect comments starting with `TODO`,
`FIXME` and `XXX` and show them there.

## Other

* Provision for different runtimes, local or remote. Compile on one runtime,
test and debug on another.

* Erlang log printouts with link to code line

## A few Eclipse tips

For those not familiar with Eclipse, some short tips and tricks:

  * **Ctrl+Shift+L** will display a list of key bindings to the
various commands

  * The Eclipse help system is pretty comprehensive, use it to find your way
around.

