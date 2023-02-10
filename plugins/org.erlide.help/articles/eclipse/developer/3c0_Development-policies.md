---
layout: article_eclipse
part: Developer's guide
---

# Development policies

## Line endings

Proper handling of line endings is very important! We don’t want to get
diffs on every line just because someone checked in CRLFs…

**Any submitted patch that has line endings problems will be rejected
automatically!**

Please make sure that you set your core.autocrlf setting properly:
`git config --add core.autocrlf input` on Linux/OSX,
`git config --add core.autocrlf true` on Windows.

## Code formatting and cleanup

Please set up the Java formatting to the Eclipse default. For CleanUp
and SaveActions, please use

- Change non static accesses to static members using declaring type
- Change indirect accesses to static members to direct accesses
- Remove 'this' qualifier for non static field accesses
- Remove 'this' qualifier for non static method accesses
- Convert control statement bodies to block
- Add final modifier to private fields
- Add final modifier to method parameters
- Remove unused imports
- Add missing 'Override' annotations
- Add missing 'Deprecated' annotations
- Remove unnecessary casts
- Remove unnecessary '$NON-NLS$' tags
- Remove trailing white spaces on all lines
- Correct indentation

## Commit policies

The rules are not set in stone (yet), we’ll change them as we
gather more experience.

- all components must compile and link, and pass regression tests;

These policies are for the official repository, of course. You can work
in your own repository as you like, the only rule is that **you** are
responsible for making all patches to apply cleanly on the official
repository. The maintainers retain the right to refuse to apply anything
that creates merge problems.

## Versioning

Because the public inter-plugin APIs are changing often, we don't want to set the version to 1.0 yet, as it would either restrict changes or make the version number go up very quickly. Other than that, the versioning scheme is using the standard Eclipse logic.

