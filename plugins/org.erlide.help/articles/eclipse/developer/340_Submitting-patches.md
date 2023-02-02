---
layout: article_eclipse
part: Developer's guide
---

# Submitting patches

Setting up user information
---------------------------

Please set up consistent user information before starting with your real
name and working\
email address on all your computers. For quick reference, here are the
needed commands:

    git config --global user.name "Your Name Comes Here"
    git config --global user.email you@yourdomain.example.com

Branching out
-------------

Create a new branch starting from `master`. Each branch should contain
logically related commits (for example the implementation of a single
feature), not a mixed bag of random changes.

Committing the patch
--------------------

-   Make separate commits for separate changes. If you cannot describe
    what the commit does in one sentence, it is probably a mix of
    logically separate changes and should be separated into several
    commits.
-   Make sure that each commit can be compiled and that it works.
-   Use the imperative mode in the commit message when describing what
    you have done: use “change”, “add”, “fix”, not “changed”, “added”,
    “fixed”. Use the present tense to describe the current
    implementation: “feature X *has* an enormous potential for
    improvements” rather than “feature X *had* an enormous potential…”
    (While this writing style may seem awkward to begin with, you’ll
    find that it is actually easier to read and write.)
- The first line of the commit message should describe concisely
    what the commit does. Leave out the full stop. (This line will be
    visible in `gitk` or in `git log --oneline`, so it should be
    understandable by itself.)
- The second line in the commit message should be blank.
- The body of the commit message should primarily answer the
    question *why?* rather than *how?*. That is, the most important is
    why the change was made, what bug it corrected or why anyone would
    want to use the new feature it implements. (Imagine yourself five
    years in the future trying try to figure why anyone would ever want
    this feature…) Technical details can also be useful, especially why
    one design was chosen rather than another design (again, imagine
    yourself investigating a bug and wondering why would anyone chose
    this way to implement this feature…).
- Line break the commit message to lines no longer than
    approximately 70 characters (otherwise they are difficult to read in
    `gitk`).
-   Do not commit out-commented code or files that are no longer needed.
-   Check for unnecessary whitespace before committing with
    `git diff --check`.

Sending the patch
-----------------

Use the “Pull request” button at GitHub.

Receiving your patch
--------------------

When we receive your patch, we will do one of the following:

-   If there are simple things that will need to be fixed (for example,
    providing more information in the commit message instead of in the
    email), we may ask you to do that.
-   Otherwise, if your patch is not obviously wrong or inappropriate, we
    will merge it.
-   Otherwise, if it is obviously wrong or inappropriate, we will tell
    you so. Reasons for immediate rejections include (but are not
    limited to):
- Blatantly breaking backward compatibility.
- Obviously unsafe coding practices or highly non-portable
    code.
- Mixing of many difference changes and/or unnecessary
    re-indentation of code that is not changed. We will ask you to
    separate the changes into separate commits and/or branches and not
    change indentation of unchanged code.

> This page is adapted from the similar page from the erlang/otp wiki,
> but any errors are entirely ours.

