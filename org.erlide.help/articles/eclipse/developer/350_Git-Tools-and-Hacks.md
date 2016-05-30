---
layout: article_eclipse
part: Developer's guide
---

# Git tools and hacks

## Windows checkout issue

As of Git 1.6.5.1 there is a special new feature for windows users. Thanks to Johan ‘t Hart, files and directories starting with a single dot (such as '.git') will now be marked hidden (you can disable this setting with `core.hideDotFiles=false` in your config) (Issue 288).

Which means if you have this version of git or newer when you checkout, all of the Eclipse project files will be marked as hidden on Windows. This makes them unwritable and causes problems.

### Solutions

-   Set the hideDotFiles property in your global git config before you checkout the project. Inside your .gitconfig file should be these lines:

  ```
        [core]
              hideDotFiles = false
  ```

-   Change the clone process so you init a local repo, change the property in just that repository, and then add a remote and pull.

## Shell Prompt

See this [guide](http://github.com/guides/put-your-git-branch-name-in-your-shell-prompt) to make git repo information show in your shell prompt.

