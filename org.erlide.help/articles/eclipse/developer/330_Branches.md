---
layout: article_eclipse
part: Developer's guide
---

# Branches

See also [Release process](3d0_Release-process.html) for details about the release process.

## Main branches

The branches from the main erlide repository.

### `master`

This branch points to the latest stable released version of erlide. It
will never be rebased.

All new feature branches should be branched from `master`.

### `pu`

This is the current development branch. It includes all fixes and new
features that we plan to include in the next release.

Here we gather all the proposed updates, so that they can be tested and
evaluated before they are include in `master`. It may be rebased, don’t
base any topic branches on it unless you need fixes/features that aren’t
present in `master` yet.

### `release`

This is a temporary branch that is used to prepare a release. It is then
merged into `master` and deleted.

## Local bugfix branches

These branches are those in your repository where the work is done and
where the result can be merged into the main branches.

Branches that fix a bug that has an issue on the ticket tracker should
have a name that starts with that bug’s id and an underscore. We would
like that the rest of the name describes the issue. This helps a lot
with having updated changelogs.

## Working on erlide

Creating new feature for erlide usually means that you will spend some
time working on it. When your job is done you will have to ask to merge
it into erlide’s repository. So here is how you should start:

- Branch from `next`. See above for when it is a good idea to use `pu`
instead.
- Develop your feature in your own branch
- Send a pull request and we will review it
- Delete your branch when the branch is merged

While developing your feature you should keep up-to-date with erlide’s
code (there maybe some changes which will impact your code or make it
very difficult to merge into `pu` when your work is done). This can be
done by merging `master` or `next`. Please don’t merge `pu` repeatedly,
as it may get rebased. Merging `pu` (or rebasing on top of it) should be
done only when you are done with your feature and want to make sure the
merge to upstream will be painless (for which I will be very grateful!).

The same workflow applies for bugfixes too.
