---
layout: article_eclipse
part: Developer's guide
---

# Branches

See also [Release process](3d0_Release-process.html) for details about the release process.

The `master` branch points to the latest changes that will be included in the next release.
It will never be rebased.

All new feature branches should be branched from `master`.

## Working on erlide

Creating new feature for erlide usually means that you will spend some
time working on it. When your job is done you will have to ask to merge
it into erlide’s repository. So here is how you should start:

- Branch from `master`.
- Develop your feature in your own branch
- Send a pull request and we will review it
- Delete your branch when the branch is merged

While developing your feature you should keep up-to-date with erlide’s
code (there maybe some changes which will impact your code or make it
very difficult to merge when your work is done).

The same workflow applies for bugfixes too.
