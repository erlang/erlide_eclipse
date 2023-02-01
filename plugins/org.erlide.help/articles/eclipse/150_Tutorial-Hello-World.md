---
layout: article_eclipse
part: Getting started
title: "Tutorial: Hello, world!"
---

# Tutorial: Hello, world!

> Thanks to Alain O'Dea for writing this guide.

## Install Erlang/OTP (if you have not already)

* _Ubuntu Linux_: run the following in Terminal:
  ```shell
  sudo apt-get update
  sudo apt-get install erlang
  ```
* _Windows_: run the Windows Installer for Erlang
* _Mac OS X_: install MacPorts and then run the following in Terminal:
  ```shell
  sudo port selfupdate
  sudo port install erlang
  ```

## Install Eclipse and ErlIDE

  1. Download Eclipse IDE for Java Developers
  2. Extract it where you want to run it from (there is no installer, just an archive)
  3. Launch Eclipse by double-clicking eclipse (Linux), eclipse.exe (Windows),
or Eclipse (Mac) in the eclipse folder
  4. Select Help (menu) -> Software Updates... (a dialog will appear)
  5. Click Add Site...(another dialog will appear)
  6. Enter `https://erlide.org/update` into the Location field, then
click OK (dialog will close)
  7. Select the checkbox by `https://erlide.org/update` and click
Install... (another dialog will appear)
  8. Select Next
  9. Select I accept ... radio button and click Finish (a progress dialog will
appear)
  10. (wait for confirmation dialog) Select Yes (Eclipse will restart)

## Add Primary Erlang Runtime

Without these steps things like syntax highlighting, code completion and other
significant aspects of the ErlIDE UI will not work properly. The IDE will
function basically, but it will not work as intended.

  1. Select Window (menu) -> Preferences... (Windows/Linux) or Eclipse (menu)
-> Preferences... (Mac) (a dialog will appear)
  2. Expand Erlang and select Installed runtimes
  3. Click Add... (a dialog will appear) and Enter Erlang in the Runtime name
field
  4. Click Browse... and select the root of your Erlang/OTP install (mine is
/opt/local/lib/erlang), then click OK (dialog will close)
  5. Click OK (preferences dialog will close)

## Create Hello World Project

  1. (wait for Eclipse to restart) Select Window (menu) -> Close All
Perspectives
  2. Select Window (menu) -> Open Perspective -> Other... (a dialog will
appear)
  3. Select Erlang and click OK (dialog will close and Erlang Perspective will
load)
  4. In the Erlang Navigator bring up the context menu (right-click/control-
click) and select New Erlang Project (a dialog will appear)
  5. Enter hello_world in the Project Name field and click Finish (dialog will
close and hello_world project will appear in Erlang Navigator view)

## Start Erlang Node To Run Code

  1. Select hello_world project, bring up the context menu and select Run As
-> Run Configurations... (a dialog will appear)
  2. Enter **`hello_world`** in the Name field
  3. Double-click Erlang application (a new confguration will appear in the
right-hand panel)
  4. In the Main tab under Projects click the checkbox beside hello_world
  5. In the Runtimes tab click the checkbox beside Start the Erlang node if
not running already and enter **`hello_world`** in the Node name field.
  6. In some environments, the cookie value needs to be specified (because
Java and Erlang don't find the same default cookie).
  7. Click Run (dialog will close and Console will appear with hello_world
Erlang node)
  8. Leave the Console running for the next part

## Write Hello World Live!

  1. On the hello_world project bring up the context menu (right-click
/control-click) and select New Module (dialog will appear)
  2. Enter **`hello_world`** in Module name field
  3. To the left of the Apply button, enter **`say_hello`** in the first box
and **`0`** in the second box and click Apply
  4. Click Finish (dialog will close and an editor for `hello_world.erl` will
be opened)
  5. In the Console type **`hello_world:say_hello().`**
  6. Oops! It displays ok and shows no greeting! Let's fix that
  7. In the hello_world.erl editor replace `ok` in say_hello with
**`io:format("Hello World!")`** and save (**Ctrl**+**S** or **Command**+**S**)
  8. In the Console type **`hello_world:say_hello().`**
  9. Great! It displays "Hello World!" for us! Let's get it to say something
else
  10. In the hello_world.erl editor replace `"Hello World!"` in say_hello with
`"Hello ErlIDE!"`) and save (**Ctrl**+**S** or **Command**+**S**)
  11. In the Console type **`hello_world:say_hello().`**
  12. Nifty! It displays `"Hello ErlIDE!"` for us! We can change the code at
runtime.

