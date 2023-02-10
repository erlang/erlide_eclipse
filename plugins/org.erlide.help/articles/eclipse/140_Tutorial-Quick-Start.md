---
layout: article_eclipse
part: Getting started
---

# Tutorial: Getting started

> Thanks to Eli Liang for writing this guide.

1. Start eclipse
2. If it prompts you to choose workspace, select the folder you want to work in. For this tutorial you might want to use a fresh one.
3. On the menu bar, Choose `Help -> Software Updates...`
4. Select the Installed Software tab. If "Erlang IDE" appears under name, then go to [17](#17).
5. Select the Available Software tab.
6. Click the `Add Site...` button on the right.
7. In the Location field, enter `https://erlide.org/update`
8. Click OK
9. Check the box left of https://erlide.org/update
10. Check the box left of Unstable builds
11. Click the `Install...` button on the right
12. When the Install dialog pops, click on the Next button
13. Accept the terms of the license agreement.
14. Click Finish.
15. Click Yes to restart.
16. Go to step [20](#20) below
17. <a name="17"/> Select (click on) Erlang IDE
18. Click on the Update button on the right
19. Follow the directions to update and restart Eclipse.
20. <a name="20"/> Click on the Workbench icon on the right side of the main Eclipse window
21. On menu bar, choose `Windows -> Preferences -> Erlang -> Installed runtimes`
22. See if there is a runtime indicated to be used by Erlide itself (restart is required). If there is something there, hit Cancel and go on to [28](#28).
23. Click on the Add... button.
24. In the Runtime name field, enter `23.3`.
25. In the Location field, browse to the very top level folder of the Erlang tree. This should be a folder called "erl23xs" or something like that.
26. Click OK. Then Click Apply and OK in the `Installed runtimes` pane.
27. On menu bar, choose `File -> Restart`
28. <a name="28"/> On menu bar, chose `Windows -> Open Perspective -> Other... -> Erlang`. Click OK.
29. On menu bar, chose `File -> New -> Project...`
30. In wizard, chose `Erlang -> Erlang Project` and click Next
31. In Project name field, type `HelloWorldProject`
32. Click Finish
33. On menu bar, choose Project and make sure that Build Automatically option is checked
34. The left pane is the Erlang Navigator. Right-click the HelloWorldProject branch. In the context menu that pops, select `New -> Other... -> Erlang -> Module` and click Next (can also just select "New Module" directly)
35. In the wizard, in the Module name field, type `hello`
36. Click Finish
37. On menu bar, choose `Run -> Run Configurations...`
38. Select the "Erlang application"
39. Click the New Launch configuration button (left side above the field with "type filter text")
40. Select the "New configuration"
41. Select the "Erlang" tab in the right pane
42. Check the box left of the HelloWorldProject
43. Select the "Runtime" tab in the right pane
44. In the field titled Node name, enter `erlide`
45. In some environments, Java and Erlang look for the default `.erlang.cookie` file in different places, and if you get connection problems you should enter some value for the Cookie field. If the node is already running, you have to use the same cookie value; otherwise, any string will do.
46. Check the box to the left of "Start the Erlang node if not running presently"
47. Click "Apply" and "Run"
48. In the center code pane (titled "hello.erl"), enter the following code: `hello/0` (within the brackets for the export, so it reads `-export([hello/0]).`
49. On the next line after the export, enter the following code:
  ```erlang
    hello() -> io:format("Hello World!~n", []).
  ```
50. In the menu bar, choose `File -> Save As...`
51. Double-click on the folder titled "HelloWorldProject" in the dialog box
52. Click on (select) the folder titled `src` in the dialog box. The field entitled Enter or select the parent folder: should now read `HelloWorldProject/src`
53. Click OK. The file `hello.erl` is saved in the src directory and automatically built.
54. The lower pane has a number of tabs. Select the "Console" tab.
55. In the console pane, enter the following command `hello:hello().` and hit return. The output from the program will appear.
56. To exit, in the menu bar, choose `File -> Close`, and then `File -> Exit`.
