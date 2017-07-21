---
layout: article_eclipse
title: Concepts
part: Reference
---

# {{ page.title }}

## Runtimes and backends

In Erlide we often use the terms _runtime_ and _backend_. Here we will try to
make clear what each of them means.

### Runtime

Describes an Erlang installation by pointing to its home directory and
optionally adding extra code path entries and/or arguments. For example, one
might want to have two R15B runtimes defined, one with and one without SMP
enabled, and be able to refer to them quickly. At the moment, this
installation must be a full installation (from the source with documentation)
but at a future time we want to support other repository formats (CEAN, Faxien).

### Backend definition

A backend definition is a set of startup parameters for an Erlang node, based on a specific runtime (as above) 
and identified by a node name. Any other command-line parameter can be specified.

### Backend

A backend is an instance of an Erlang node described (possibly started) with the parameters specified by 
a backend definition. It has erlide-specific code running on it.

There are two kinds of backends: managed and standalone. The Erlang node
behind a **managed** backend is started and stopped by Erlide. The one behind
a **standalone** one has an independent life cycle. A managed backend's
definition must include a runtime; for a standalone one, the reference is
informative only since the Erlang node is already started.

There are several kinds of backends: ide, build and execution. A single Erlang
node may very well be behind several backends, but only at most one of them
must be managed.

* IDE backend -- Used by Erlide itself for all IDE related functionality (lexing, parsing, etc)
* Build backend -- Each project references a target version for its build target and OTP library sources.
* Execution backend -- The execution backend is where the code is loaded and run/debugged. It is
specified in an Eclipse run or debug configuration.

- The build and execution project backends must use compatible runtimes. I.e. `version(build) <= version(execution)`.
- A backend can be shared by multiple projects.
- A backend can be managed or external.
    * __managed__: it is started and stopped by the current instance of Erlide. The IDE and build backends 
    are always managed.
    * __external__: has a life-cycle independent of this instance of Erlide. It may be available (running) or not. 
- Availability of external backends will be monitored and functionality will automatically follow the life 
cycles of the backend.
- Unavailability of the backend must not affect normal editing or non-erlide operation in Eclipse.
- Managed backends are started when a project using them is opened (unless they were already running). They 
keep track of the projects using them and will not shut down until all these projects are closed.
- The version of the runtime associated with an external backend will be compared to the actual version 
running on the node and they must match. If not, the backend will remain unavailable.

#### Configuring backends

* Runtime definitions and backend definitions are configured globally inside the workspace. We might consider 
in the future making the definitions available per Eclipse instance.
* In most cases, the user should only install runtimes in Eclipse. Some default definitions will be created 
automatically. Advanced users will be able to create additional ones.

> **A note about cookies** <a name="cookies">
> 
> In some environments, Java and Erlang don't look for the default
`.erlang.cookie` file in the same places. This will result in failure to
connect the two nodes. There are two ways to solve that:
>
> * check where the two VMs look for the file and make sure that identical
copies are found in each place. Usually this happens if the `HOME` variable points 
to some other place than the real home, for example to `H:\` instead of `C:\Users\your-name`
> * enter some value in the cookie field. If the Erlang node is already
running, then of course the entered value should be identical to the one it
uses.