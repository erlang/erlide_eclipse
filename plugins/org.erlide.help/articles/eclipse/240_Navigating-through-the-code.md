---
layout: article_eclipse
part: Reference
title: Navigating through the code
---

# {{ page.title }}

There are several ways to find what you are looking for in a code base.

* `Ctrl-Shift-M` open module

  Opens a dialog where you can start entering the name of the module you want to open. Usually a few characters are enough, select it from the list and press enter.

* `Ctrl-Shift-R` open resource

  Same thing, but works for any file (text, java, asn1, idl or whatever).

* `Ctrl-H` search

  The Erlang search lets you search for functions/records/macros, as references, definitions or both.

* `Ctrl-G` and `Ctrl-Shift-G` find references/definitions

  Shortcuts for searching references and definitions for the element at the cursor, in the current editor.

* `F3` and `Ctrl-click` open definition

  Go directly to the definition of the element at the cursor.

* `Ctrl-O` quick outline

  Opens a module outline where you can start typing to filter the elements you are interested in.

* `Ctrl-F`, `Ctrl-J`, `Ctrl-Shift-J` text search

  Textual search in the current file. The latter two perform incremental search for the current selected text or if none, for text being entered.

