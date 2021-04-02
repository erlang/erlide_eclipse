---
layout: article_eclipse
part: Reference
title: Configurating a project
---

# {{ page.title }}

> TODO: outdated!

In the Navigator pane, right-click on the project and choose `Properties ->
Erlang`. The dialog allows you to specify the project's settings.

![Project properties](images/projectproperties.png){: .frame }

The build backend specifies the Erlang node on which the compilation will be
done. By default, we use the same backend as the one the IDE is using and in
most cases this is sufficient. See the [Concepts](210_Concepts.html) section for more details.

The "make unique" box lets you decide if the specified name will get a unique
suffix in order to ensure that there will be no conflict with other running
nodes (the use case is when there are several users running Erlide on the same
machine, it's difficult to ensure that they won't use the same name for
nodes). The suffix is derived from the workspace path, so it is deterministic.

An empty cookie field means that the user's default cookie will be used.
