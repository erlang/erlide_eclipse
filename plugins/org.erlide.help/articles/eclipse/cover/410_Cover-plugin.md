---
layout: article_eclipse
part: Cover plugin
---

# Cover Plugin

Cover plugin is an integration of Erlang cover tool with ErlIde. It provides a friendly graphical user interface that helps configuring and viewing coverage analysis for your project. That is how it simplifies a process of testing Erlang projects developed with ErlIde.

To learn more about cover click the following link [https://www.erlang.org/doc/apps/tools/cover_chapter.html](https://www.erlang.org/doc/apps/tools/cover_chapter.html). Cover reference manual can be found here [https://www.erlang.org/doc/man/cover.html](https://www.erlang.org/doc/man/cover.html).

Cover plugin was developed as a part of [ProTest project ](https://www.protest-project.eu/)  by [Erlang Solutions ](https://www.erlang-solutions.com/) .

## Features
*  Providing coverage statistics (coverage ratio per function, module, source folder, project) 
*  Marking coverage in the editor
*  HTML reports generation (HTML reports can be viewed both from Eclipse or can be exported and saved)
*  Saving and restoring coverage results
*  Browsing coverage marking for specified modules or functions
*  Opening items from statistics view

Cover plugin is meant to be integrated with any testing Plugin (EUnit, Common Test, QuickCheck) through Cover API plugin (for details see: [https://github.com/esl/erlide/wiki/Cover-notes](https://github.com/esl/erlide/wiki/Cover-notes.html))

## Implementation details

If you want to understand the source code go [here](430_Cover-plugin-implementation.html)

## Examples

You can find examples how to use this plugin [here](420_Cover-plugin-example.html)
