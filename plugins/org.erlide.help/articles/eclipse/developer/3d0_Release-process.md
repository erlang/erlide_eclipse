---
layout: article_eclipse
part: Developer's guide
---

# Release process

We are planning to release a stable version often, around once a month or a little more often.

See [Branches](330_Branches.html) for a description of the branches in the erlide repository.

Whenever a fix is done or a feature is implemented, it is merged in `pu`. At the time for a release, the following actions happen:

- `master` branch is moved to the current `next`
- fixes and features from `next` that have been tested as stable and complete are merged into `master`
- if some fixes were rejected in the step above, rebase `next` on top of `master`
- reset `next` to `master`
- fixes and features from `pu` that are deemed stable and complete are merged into `next`
- `pu` is rebased on top of `next`
- `later` is rebased on top of `pu`

Then a stable release is built from `master` and an unstable release from `next`.


