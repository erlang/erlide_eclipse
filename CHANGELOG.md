## 0.33.0 (20160524)

- **last version supporting Java 6**
- remove debug printout in ErlModule
- configure logger output: erlide.logger.console.debug
- add support for OTP 19 (+kernel)
- fix problem reading rebar config
- fix problem with reading builder configuration
- return the right version for a project's OTP library
- update supported versions
- fix target and deps for indigo
- use single kernel plugin

## 0.32.1 (20160422)

- **last version supporting OTP R15**
- configure erlide log via system property

## 0.32.0 (20160414)

- [#261](https://github.com/erlang/erlide_eclipse/issues/261) - search installed runtimes in $PATH too
- append RpcMonitor stats to erlide.log instead of a separate file
- make location of erlide log configurable
- output only warnings and errors in the console
- [#263](https://github.com/erlang/erlide_eclipse/issues/263) - Add action to clear all model caches

## 0.31.0 (20160404)

- indentation: add 'indent width' parameter
- move help content to erlide.github.io
- [#260](https://github.com/erlang/erlide_eclipse/issues/260) - disable invalid runtimes
- move tracing and cover plugins to erlide core; erlang code to kernel
- move all erlang sources to an own repository; keep only beams here
- update jinterface to 1.6.1
- add handly to targets
- update xtend libs to 2.9.1
- [#246](https://github.com/erlang/erlide_eclipse/issues/246) - runtime version numbers may use _ as delimiter

## 0.30.1 (20160316)

- Merge hotfix 0.29.14

## 0.30.0 (20151103)

- [#235](https://github.com/erlang/erlide_eclipse/issues/235) - recognize more types of version numbers
- [#241](https://github.com/erlang/erlide_eclipse/issues/241) - remove headless feature
- [#238](https://github.com/erlang/erlide_eclipse/issues/238) - cache hostname values to speed up startup
- debugger: catch missing bundles
- [#233](https://github.com/erlang/erlide_eclipse/issues/233) - improve launch "runtime" tab to make it clearer
- [#230](https://github.com/erlang/erlide_eclipse/issues/230) - detect kerl and its installations

## 0.29.14 (20160315)

- [#246](https://github.com/erlang/erlide_eclipse/issues/246) - runtime version numbers may use _ as delimiter
- [#235](https://github.com/erlang/erlide_eclipse/issues/235) - recognize more types of version numbers
- [#233](https://github.com/erlang/erlide_eclipse/issues/233) - improve launch "runtime" tab to make it clearer
- [#259](https://github.com/erlang/erlide_eclipse/issues/259) - clean debug printouts

## 0.29.12 (20150710)

- [#229](https://github.com/erlang/erlide_eclipse/issues/229) - epp error, unable to build automatically
- [#228](https://github.com/erlang/erlide_eclipse/issues/228) - create github release and upload zip from cli
- add debugger plugin för OTP 18
- [#222](https://github.com/erlang/erlide_eclipse/issues/222): simplify "new module" dialog
- treat "test" dir as source if in test mode
- Merge pull request [#220](https://github.com/erlangrge/erlide_eclipse/issues/) from zsoci/debug-int
- Have the source found where it is defined in the beam when default search fails.
- add navigator filter for beam files
- fix display of multiline compiler error messages
- update default backend version to R17
- replace usages of erlang:now as per OTP 18 recommendations
- move "erlang tools" menu to ui plugin
- fix NPE when terminating backend

## 0.29.11 (20150216)

- fix stupid mistake
- fix bug in term parser (lists, tuples and maps did not require commas)

## 0.29.10 (20150206)

- 1376,1044 - make node proxy service restartable
- print error when loading beams fails

## 0.29.9 (20150112)

- create kernel.tools plugin, for dialyzer and other tools

## 0.29.8 (20141216)

- load all debugger code on remote nodes
- hotfix external includes

## 0.29.7 (20141214)

- [#207](https://github.com/erlang/erlide_eclipse/issues/207) - Code reloading on remote nodes
- [#208](https://github.com/erlang/erlide_eclipse/issues/208) - Problem loading on remote nodes

## 0.29.6 (20141121)

- [releng] update buckminster and build erlang only with rebar
- update graphviz reference
- hotfix NPE in editor
- don't call kernel.ide stuff from headless code

## 0.29.5 (20141003)

- show console after launching Erlang app
- add tooltips to wizard buttons in toolbar
- handle partial versions, like 17.4-rc1
- don't crash if no runtime can be found at startup
- console: specify and recognize encoding for printed-out values
- 1372: "Source directories" in project properties is whitespace sensitive
- [#198](https://github.com/erlang/erlide_eclipse/issues/198): OTP version is not properly detected

## 0.29.4 (20140916)

- move erlide_util to kernel.common: fix code execution problem

## 0.29.3 (20140915)

- (1370,1371) extract generic debugger code in separate plugin
- update jinterface from OTP
- model: add libraries as elements

## 0.29.2 (20140908)

- update erl_scan from latest OTP (column fixes)
- updated Xtend to 2.7.0
- extract erlang builder that doesn't depend on eclipse framework
- better shutdown logging
- [#193](https://github.com/erlang/erlide_eclipse/issues/193): -spec did not work
- console refactoring
- 1363: save runtime preferences under a different key
- eclipse 4 adjustments

## 0.29.1 (20140625)

- fix problem with internal BundleURLConnection

## 0.29.0 (20140623)

- [#189](https://github.com/erlang/erlide_eclipse/issues/189) - stop an erlang shell connected to an external erlang node
- fix synchronization issue when starting shell
- handle crashing ide runtimes better
- fix in jinterface: better exception messages
- console: copy/paste works now in both fields
- improve multiline handling; still some things depend on scanner fix
- adjust indentation for concatenated strings
- [#184](https://github.com/erlang/erlide_eclipse/issues/184) [#185](https://github.com/erlang/erlide_eclipse/issues/185) - indentation issues with record/map as first parameter
- [#183](https://github.com/erlang/erlide_eclipse/issues/183) - Named fun indentation broken

## 0.28.2 (20140512)

- convert all xtend files to utf-8
- [#181](https://github.com/erlang/erlide_eclipse/issues/181) - don't show crash report for execution backends
- [#182](https://github.com/erlang/erlide_eclipse/issues/182) - console title shows if launch mode is "debug"
- add "goto matching bracket" command (Ctrl-Shift-P)
- add debug printouts for 1353
- workaround for slow input in console
- allow typespec with specified module
- edoc: properly detect typespec before function
- add icons for macros, records
- 1303 - autodetect source directories
- 1300 - builder traverses all source dirs looking for .app.src and modules

## 0.28.1 (20140506)

- 809 - disable autocompletion chars in string literals
- [#152](https://github.com/erlang/erlide_eclipse/issues/152) - Toggle comment (Ctrl-/) scrolls the editor
- 1239 - completion: proposals should close after dot or semicolon
- 1315 - Toggle comment (Ctrl-/) scrolls the editor
- [#171](https://github.com/erlang/erlide_eclipse/issues/171) - Indentation for "after" clause in try/catch seems off
- [#176](https://github.com/erlang/erlide_eclipse/issues/176) - Comment code CTRL-SHIFT-C works strange
- [#175](https://github.com/erlang/erlide_eclipse/issues/175) - code completion is case sensitive
- Build otp model only once and cache it better
- wrangler: make dependency on graphviz optional
- publish latest version number in plain text

## 0.28.0 (20140403)

- rename editor tab width preference to 'indent width' and make read-only
- 1307 - better parse record definitions with type specs
- 1344 - builder: limit number of parallel compiles
- when runtime dies/stops, remove it from manager
- [#168](https://github.com/erlang/erlide_eclipse/issues/168) - debugger: send source paths to int:i
- implement quickfixes
- [#166](https://github.com/erlang/erlide_eclipse/issues/166) - fix encoding issue in console
- improve debug info when engine doesn't start
- .app generation: ignore modules in deps/ and not directly on source dirs
- don't sort applications when writing .app file
- only recreate .app if there is a change
- don't touch app.src if erlide.no_app_src is defined
- [#166](https://github.com/erlang/erlide_eclipse/issues/166) - Erlide Eshell appears to hang
- [#163](https://github.com/erlang/erlide_eclipse/issues/163) - correct location of log in message shown to user
- [#162](https://github.com/erlang/erlide_eclipse/issues/162) - Incorrect auto-close braces/brackets/parenthesis
- new wizard: set minimum req OTP version to r15
- initial work on building with rebar/make/emake (not enabled yet)

## 0.27.1 (20140311)

- move erlang files to kernel.common

## 0.27.0 (20140310)

- debugger: update debugger code for r15 and r16
- debugger: update to latest from r17
- update jenkins tooling for otp 17
- make code loading from plugins respect code context and version
- dispose backends only once
- run initialCall only on managed backends
- dialyzer: can run with both long and short names

## 0.26.1 (20140305)

- 1332 - debugger: hangs when started multiple times

## 0.26.0 (20140221)

- open modules: sort items to start with better matches
- reenable "run as" on project explorer
- make scanner work with unicode files
- completion: templates match case-sensitively
- upgrade min erlang version to R15/R16
- 339 - use the OTP lexical scanner
- add getBinary and getAsString to Bindings
- make sure R17+ projects get utf-8 encoding
- add some higher-level support for maps
- add jinterface support for maps
- 1306 - erlide cannot handle quoted module name correctly
- indenter accepts r17 maps now without crashing

## 0.25.0 (20140207)

- debugger update; not all r17 stuff is in yet (maps, named funs)
- update guava to 16.0.1
- add support for r17-style version numbers
- update to xtend 2.5.1
- fix RuntimeVersion to support OTP R16B03-1
- let VersionLocator return even older versions (lowest priority)
- fix content describer
- guard agains problems in jrpc notifications
- improve logging output
- [#150](https://github.com/erlang/erlide_eclipse/issues/150): fix detection of file encoding
- when ide runtime points to inexistent install, search for a useable one

## 0.24.2 (20131209)

- fix hanging search when parser crashes

## 0.24.1 (20131129)

- after changing runtime, ask user to restart
- fix crash in ModelActivator when no runtime is configured
- [#134](https://github.com/erlang/erlide_eclipse/issues/134): if only one runtime, selection behaves strangely
- 1300 - disabled .app.src handling because of inefficiency
- remove debug logging (gh 133)
- 1258 - completion: suggests module names that are regular files from src
- 1290 - detection of open targets can fail
- 1278 - new erlang module: "new_file" needs to be removed manually
- [#129](https://github.com/erlang/erlide_eclipse/issues/129) fix code templates
- Fix a race condition in Console, resulting in empty output pane sometimes
- add test file for eep37 (named fun expressions)
- 1267 - searching---filling-in-record-field-into-search-dialog-from-current-selection-goes-wrong
- add error dialog when we can't connect to backends because of hostnames
- [#124](https://github.com/erlang/erlide_eclipse/issues/124) - add manual configuration of host names
- 1263 - value for ide runtime can be ignored
- rename "add nature" menu to "mark as erlang project"; similar for remove
- Fix of erlang compare-mode problems (Unique scanner-names for file-less modules)

## 0.24.0 (20130909)

- Cache external OTP structure
- 1254 - macro definitions not found if space between name and comma
  also: a lot of internal changes that make maintenance much easier
  also: starting to create an API for the code-handling engine

## 0.23.2 (20130823)

- 1250 - io:format statements output is not ordered
- [#120](https://github.com/erlang/erlide_eclipse/issues/120): fix stack overflow when terminating debugged node
- don't report runtime down when shutting down
- [#117](https://github.com/erlang/erlide_eclipse/issues/117): fix stack overflow when terminating debugger
- add product feature
- fix problem with dispatching events after shutdown
- create crash report even for exit code = 1, just in case
- make sure beam processes are killed when checking hostname compatibility

## 0.23.1 (20130620)

- better looking local documentation in hover
- add ErlideEventBus to allow communication between plugins.
- handle NPE when shutting down
- 1240: run as erlang application can choose wrong node name type
- improve RuntimeTab behaviour when not both long/short names can be used
- fix NPEs in ProjectPreferencesWizardPage
- Removed ErlExternalReferenceEntryListProxy, it complicates things and does not give a significant performance increase
- Make sure ErlProject is removed when Project is deleted
- update xtend libs to 2.4.1
- remove conflicting key binding C-A-S-M
- remove double 'open' and 'search' menu items
- remove backend.api project, is not needed
- fix for scanning $\n, was incrementing the row too
- update requirement for eclipse.help.ui plugin that has been upgraded to 4.0 in Kepler
- change default time for code assist activation to 50ms (from 100ms)
- 1214 - completion: external includes are proposed as modules
- 1220 - hover: format internal docs and specs
- improve documentation presentation (remove leading %%); part of 1220
- 1215 - e4: refs to inexistent actions makes eclipse unusable

## 0.23.0 (20130516)

- 1208 - completion: optimize search for suggestions
- 1210 - completion: propose builtin types
- 1196 - distribute cache files in multiple directories: use same directory structure as in workspace
- 1131 - long delay at startup; not sure this is enough
- 1204 - use eclipse status dialog instead of custom one
- [#107](https://github.com/erlang/erlide_eclipse/issues/107): templates invalid
- fix ctrl-space completion in console
- console: fix completion docs and remove esc command
- 1200 - searching - selecting record name in declaration searches for record field
- rpcs fail faster now if backend is down (no timeout, just throw exception directly)
- 1183 - update .app files with current info from the project
- 1187 - search references: skip searching outside the module if the function is not exported
- 1192 - search: the search window (ctrl-h) doesn't work for variables

## 0.22.0 (20130418)

- throw exception if setting an illegal node name (instead of changing it quietly)
- better fix for choosing long/short name: launch config decides
- remove unused libs
- [#44](https://github.com/erlang/erlide_eclipse/issues/44): fix problem with syntax highlight dialog
- allow running ide backend with short hostname (use -Derlide.shortname)
- add more debug printouts when memory gets low
- dialyzer: fixes in DialyzerMarkerUtils
- mark occurences - sometimes wrong Was actually the cache directory that was different, sometimes core, sometimes model
- Reuse structure of externals and OTP using proxies in the model tree
- 600 - Support .app file for project
- console sends input with Enter only if at end of input. Otherwise Ctrl/Option-Enter works as before
- improved startup time a bit
- upgrade eclipse target to 3.7
- 1180 - Error on running Cover run configuration.
- move guava libs to libs project; upgrade xtend libs to 2.4.0
- 1177: open runtime preferences dialog if no runtime is configured
- 1178 Sometimes cannot cancel http://assembla.com/spaces/erlide/tickets/1178-search---sometimes-cannot-cancel Tricky bug when setting up parameters to search
- Reuse structure of externals and OTP using proxies in the model tree

## 0.21.2 (20130322)

- add debug printout when opening stuff in navigator takes long time
- 1006 - Syntax Coloring doesn't update open editors
- Make parsing and scanning works even for modules without path (e.g. tests and compare)
- 1168 - use ide backend for OTP libraries, if same version
- 813 - "externals" and "otp" folders on non-erlang projects
- debug problems in 1163 1164
- update jinterface to R16
- fix issues reported by FindBugs
- update 3rd party libraries
- Refresh gives double outline http://assembla.com/spaces/erlide/tickets/598-refresh-gives-double-outline

## 0.20.2 (20130311)

- 1165 - crash when starting and no runtime is found
- 1166: launching can fail, using wrong kind of node name

## 0.20.1 (20130307)

- 1158: crash in ErlFolder.isOnPaths
- debug open_externals_tree timeout

## 0.20.0 (20130304)

- fix another synchronization issue in reconciler
- 1157: Erlang editor extremely slow entering <Enter>, ".", "," and ";" key presses
- github 98: if ide backend has no docs, a crash prevents completions to show up
- better fix for 1154
- 1154: IErlElementLocator.findModule(String moduleName, String modulePath) cannot find module if modulePath starts with "c:" (on Windows)
- 1153: ErlUtils.isAccessibleDir(IRpcSite backend, String localDir) deletes empty localDir
- project explorer: module model is not updated
- Scratchpad working with latest pu
- github 97: renaming runtime can throw exception
- Indent failed in catch with guards
- extract dialyzer plugins (note that dialyzer builder is now disabled)
- 1145: editor can have two reconcilers running
- 1143: Erlang application Run Configuration settings on Runtimes tab are blank after updating from 0.18 to 0.19
- 1142: builder: errors are shown for one file only when using "build automatically"

## 0.19.0 (20130208)

- remove "backend down" dialogs when stopping debugged runtime
- filter out too old runtimes from the list
- merge model refactoring branch
- 1118: add preference to enable/disable scanning for TODO comments
- 1138: selection of runtime by version always picks the latest
- 1137: NPE adding Erlang Runtime
- 831: fix jinterface default cookie handling on Windows
- update jinterface to R16A
- 1127: cyclicpath exception in navigator
- fix console to work with R16
- improve cold start times (not checking minor version of runtime all the time)
- disable cheatsheet, content is malformed
- 1103: NPE when doing global search
- 145: Color preferences dialog: "enable" box is confusing

## 0.18.0 (20130121)

- 1122 completion: no suggestions after guard with macro
- 1123: scanner gets out of sync after completion
- Merge pull request [#92](https://github.com/erlangrge/erlide_eclipse/issues/) from vladdu/scanner: fix problem with eternal scanner processes eating up memory
- Merge pull request [#91](https://github.com/erlangrge/erlide_eclipse/issues/) from jakobc/noparse: separate scanner and parse functionality to make it easier to test
- 1116: indent fails for large blocks of test; increased timeout of this slow operation
- Fixed sync problem There was code between getting from reconciler queue and removing from it. If regions were added from another thread (e.g. from user typing), the scanner buffer and the editor buffer would diverge.
- make BackendData part of API
- rename jinterface plugin to "runtime"
- create backend plugin and make jinterface plugin independent of eclipse
- fix NPE in new file wizard
- remove periodic dump of backend info; not needed anymore
- 1114: console problem with dark colors.

## 0.17.7 (20121214)

- debug backends not being shut down
- 798  debugger: live code update
- set the default file encoding to whatever the workspace says
- Set "module" template to default when creating new erlang module
- Better handling of search-server state
- fix NPE in edoc hover
- fix edoc css to specify foreground color
- 1029-stale-warnings-don-t-clear-on-recompile-
- Fixed problem with "Open Declaration" when navigating within docs (Edoc and Hover)
- Removed "Goto Input" text from edoc-view, enabled open declaration

## 0.17.6 (20121206)

- simplify and improve process heap monitoring
- 1096: search_server kills VM because of too much memory usage
- add debug info to hostnameRetriever
- 1093: task reminders are created at build only
- 1063: docs: navigate links in OTP docs

## 0.17.5 (20121126)

- content describer: accept case insensitive encoding tags
- 1077 hover: show -spec information
- Merge branch 'hover' into pu
- 1097 hover: add preference to disable showing it
- 1092 default folder for new module is wrong
- 1078 completion: quoted record names not shown after the first quote
- Links in hover and external browser
- 1083-indentation--bad-after--spec-with-when-clause
- indentation: receive..after is wrong
- New InterpretedModulesView.java with List of modules instead of tree
- 1087: improve docs for troubleshooting backend that can't be started
- 1086: better backend hostname detection
- let PopupDialog display html, with links and all
- 1084: if internal backend fails to connect, don't keep trying [[#78](https://github.com/erlang/erlide_eclipse/issues/)]
- content describer usessues the R16 way, %% coding: Latin-1 or UTF-8

## 0.17.4 (20121015)

- 1080: debugger: doesn't shut down on remote targets
- use long names by default for internal backends; if not possible, try short names
- by default, load project code on all nodes
- added experimental support for -encoding(E) attribute, with E=latin1|utf8
- retrieve real erlang host names
- add NetworkPreferencePage
- add /usr/lib64 to the list of default runtime locations
- better handling of unknown debugger messages
- automatically detect all debugger modules to be distributed to other nodes
- use better way to get the loopback interface's ip address

## 0.17.3 (20120920)

- updated module_templates, nicer formatting
- use localhost as name for the current machine when checking epmd
- make "backend died" message non-modal, which is less intrusive
- 1075: fix problems with hostname on osx
- removed xtend libraries
- update jinterface to R15
- v0.17.2
- add debug printout when setting erlide runtime
- update editor templates [#77](https://github.com/erlangdate/erlide_eclipse/issues/) (thanks to Bjoern Kortuemm)
- add contributors file

## 0.17.2 (20120913)

- fix NPE in autocompletion ([#74](https://github.comx/erlang/erlide_eclipse/issues/))
- fix wrangler stuff; usage of ArrayList and extra incava.Diff classes
- correct spec to use ets:tid() instead of tid()
- Debugger from R15B01 merged into erlide
- fix NPE when erl fil is not in a project (shade 2577)
- update minimum supported Erlang version to R14
- avoid NPE at shutdown after debugging
- use 'localhost' as hostname when erlide.longshort.hack is defined
- add back check for erlide.test_builder.disabled, that had been removed by mistake
- having duplicate module names is now a warning

## 0.17.1 (20120731)

- 1066: mark occurrences: not working when file is just opened
- fix NPE in module search when there are symbolic links
- 272: debugger - still problems with "debugged" tab in launch config. dialog
- use standard OTP css to display edocs
- 1060: handling of utf-8 comments and strings
- 1057: cannot load code on nodes started in embedded mode
- 1056 erlang compare - NPE when pasting in compare editor
- add option to start backend with an external "full" console; no gui yet, use -Derlide.backend.detached=true

## 0.16.1 (20120617)

- fix 91-distributed-debugging
- don't start epmd, it might hang up eclipse
- 1032-indent-(ctrl-i)-fails-if-file-contains---spec--statements

## 0.16.0 (20120430)

- 1046: Add back parse_transform in compiler options page
- 1042: run "reset caches" automatically at midnight
- 1041: minimize logging when backend dies
- 1043: kill beam process when node gets disconnected
- 1039: show "please restart" message when disconnecting from backend
- moved guava library to a separate plugin
- added test in Stringutils (1030)

## 0.15.3 (20120423)

- 1031-shade-bterl--builder-is-disabled-because-of-bt_parse-transform
- 1024/51: otp edoc shows strange chars
- 1027 import project: error when no beam files exist
- 1026: console: can't choose colors
- added a simple function to get the length of utf8 encoding strings which contain characters other than ascii chars (thanks to Cao Xu)
- 1022 outline: "Filter..." menu is added even to the Java outline

## 0.15.2 (20120315)

- made internal features configurable at runtime
- added debug for ticket 1017
- 1014: make font size configurable for hover popups
- fix shade [#2451](https://github.com/erlang/erlide_eclipsex/issues/): use official bterl tool from cc
- 890: outline filters not effective when opening file

## 0.15.1 (20120220)

- fix layout for runtime tab
- 890-outline-filters-not-effective-when-opening-file
- 64: Compiler can't be configured
- fixed color settings preview to display the colors
- 999: disable compiler options that don't have any function yet
- 977: less functionality for large files for better performance; for folding only

## 0.15.0 (20120130)

- 834: searching: still problems with search on function call
- 956-compare-view-(structure)-shows-wrong-diff
- 955-editor-doesn-t-support-a-resource-s-history-versions
- 989: Dialyzer failing
- 983: include wrangler.codeinspection fragment; requires Graphviz plugin
- finished updating minimum requirement to R13

## 0.14.0 (20120105)

- 947: completion of include and include_lib
- 971 navigation - should go from -spec to function
- 981 model sync: getModelLock is not used everywhere
- fix for console history when there was no previous input
- 976 scanner: doesn't recognize all escape sequences in char literals
- erlide runtime is always the most recent stable one, user can't choose it anymore
- fixed 979 (search for a function without specifying arity uses zero as default), but results are affected by ticket 984
- found problem with highlighting in history files (but not the root cause)
- test [#975](https://github.com/erlang/erlide_eclipse/issuesst/): display dialog when backend dies
- test [#974](https://github.com/erlang/erlide_eclipse/issuesst/): improve printouts when backend dies

## 0.13.9 (20111212)

- 962: indent macros in list comprehensions
- merge latest wrangler updates
- updated to require java 1.6
- 946 erlide_otp_docs_x uses the deprecated regexp module removed the module, it's not used
- Outline when opening a local history file Still no highlight in local history or compare
- builder: dont delete beam file if no erl exists in source directories
- included fix for jinterface https://github.com/NicoK/otp/compare/jinterface_better_buffer_alloc
- partial fix for [#914](https://github.com/erlangrtial/erlide_eclipse/issues/): slow edit for large files -- autoindentation is improved
- backend: start peer node first, so that epmd is running
- external file in editor will not open after restart 951/
- changed console UI to address several problems with it
- 961: console: keep input field open when losing focus
- 962: console: switch meaning of Enter and Ctrl-Enter
- 952-mark-occurences---no-mark-when-on-local-call
- shorten names for node when projects have common prefix
- disabeld auto newlines in strings, comments, chars

## 0.13.8 (20111117)

- fix crash in RpcMonitor
- fix out of memory crash (error when registering event handler)
- 954: highlighting chars is incomplete
- add character scanner as partitioner, otherwise it interferes with strings and atoms

## 0.13.7 (20111110)

- 950-content-assist-shows-empty
- Lock model when updating
- 944-unhandled-event-loop-exception-java-lang-stackoverflowerror
- Merge remote-tracking branch 'esl/pu' into pu
- Better handling of include_lib completion and navigation
- 938-improved-performance-of-syntax-highlighting
- Some bugs fixed; added images to label provider
- added RpcMonitor: we can now gather rpc statistics
- change text for exception when detecting UTF-8 files
- workaround for bug in java.lang.String http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6242664
- fix 934-eclipse-install-erlang-plugin-error
- 891-wrong-function-comment-in-edoc-view-and-hover
- fix https://github.com/erlide/erlide/issues/15
- 933-searching--can-t-find-function-ref-after-record
- use osgi events for erlang events

## 0.13.6 (20111005)

- 613-dialyzer--progress-indicator
- 913-concurrentmodificationexception-while-editing-large-files
- 613 dialyzer - progress indicator
- 707 document "toggle erlang nature" command

## 0.13.5 (20110913)

- 901-exclude lost+found from build
- 910-test_builder--directory-links-make-it-confused
- 906-test_support--npe-if-failure-cause-is-not-an-atom
- 905-test builder doesn't run properly
- 912-epmd-can-start-too-slowly--backend-crashes
- 834-searching--still-problems-with-search-on-function-call
- 909-build-error-(in-ups-project)

## 0.13.4 (20110823)

- 899-capture-output-of-make_links-for-debugging enabled with -Derlide.make_links.snoop=true
- 898-add-switch-to-create-task-markers-only-from-menu
- 895-testsourcepathprovider-is-too-slow-
- wrangler: Using existing name as default in 'rename' refactorings
- fix default log directory to user's home dir
- 866-navigation--functions-defined-in-hrl-files-are-not-found
- added option to disable test code builder, -Derlide.test_builder.disabled=true
- 839-erlide-log-file---specify-directory
- 874-erlide-nullpointerexception-when-trying-to-compare-to-previous-version
- 432-cannot-paste-into-console-window-unless-you-first-type-another-char
- 732-navigation--merge-open-and-hover-code

## 0.13.3 (20110701)

- Fix bug with multiple plt-files in dialyzer run.
- check for null project when cleaning backend
- Update PLT with new paths Updating even when original beam files are missing.
- 838-implement-plt-copy-feature
- sometimes the runtime is accessed before being initialized

## 0.13.2 (20110613)

- Update PLT with new paths
- better fix for huge dialyzer error handling
- Made sure warnings shows up on resources
- 608-dialyzer---navigate-to-external-includes-from-markers
- 864-clean-project-does-not-remove-files-in-ebin-directory
- https://redmine.shade.psgbg.se/issues/2100 (Wasn't this fixed once before?)
- 830-completion--show-message-when-no-completions-are-available
- 852-unicode-content-is-mangled-in-documentation-hover
- 828-new-project-wizard---runtime-combo-is-empty
- 862-new-project-wizard-sets-output-dir-to--[ebin]
- sometimes epmd starts too slowly; retrying connection
- Runs from own copy of dialyzer_cl to prevent check of plt and beams
- upgraded minimum erlang requirements to R13B

## 0.13.1 (20110527)

- Update dialyzer plt with .beam files in ebin directories.
- Avoid too long error messages (> 30K) breaking down eclipse
- dont display symlinks in OpenModule
- Outline disappeared from modules not in project (again)
- made outputDirs a list (prepare for [#847](httpsde://github.com/erlang/erlide_eclipse/issues/)); current API is deprecated
- 845-test_support--test-modules-are-not-shown-in--open-module-
- 844-regression--f3-doesn-t-work-in-0-13-0
- 844-regression--f3-doesn-t-work-in-0-13-0
- 839-erlide-log-file---specify-directory

## 0.13.0 (20110517)

* There are again some large refactorings of the API code, and we're not yet close to something stable.

- 838-dialyzer---implement-plt-copy-feature
- 814-npe-in--open-module--dialog
- 722-completion--nothing-is-shown-for-modules-using-export_all
- 752-test_support--improve-results-view-ui
- 750-test_support--links-to-source-from-results-view
- 549-open-module-dialog---too-slow The reading of resource attributes slowed down the searching a lot... removed

## 0.12.0 (20110503)

- 826-launch---quick-launch-does-not-work-on-windows-some-times
- Fix so that debugger handles arguments with small integers (they are given as OtpErlangString)
- 797-model---the-splitting-into-function-clauses-is-too-simple--causing-bad-code-formatting
- 824-outline---sometimes-no-text
- 549-open-module-dialog---too-slow revisited.
- 819-searching--cancel-search
- changed Cover plugins names

## 0.11.1 (20110412)

* added cover plugin, thanks to Aleksandra Lipiec

- 712-import-erlang-project---link-option-support-should-be-rewritten
- 815-extra-args-for-erlang-vm-doesn-t-work
- 560-erlang-search---should-run-in-background
- fixed NPE when file was not in erlang project
- 784-searching--separate-checkbox-for-otp
- 758-new-module---selects-non-existing-folder
- 808-open-module--problems-with-closed-projects

## 0.11.0 (20110322)

for developers: more API have changed; we added many more unit tests.

- 765-navigation--hrl-file-found-instead-of-erl
- 793-testing---create-test-projects
- 794-testing---make-a-simple-test-case-on-test-projects
- updated jinterface to 1.5.4 (R14B02)
- 804-console-buttons-are-disabled
- show "wait" cursor while bterl compiles files
- 710--compile-file--action-to-show-notification
- 763-erlang-model---cache-problems First attempt, more tests needed...
- 491-add-action-to-run-erl_tidy-  (disabled, because reformatting doesn't work yet)


## 0.10.2 (20110301)

for developers: the APIs have changed and they will keep changing, hopefully to something that is actually usable.

- 787-indent---confused-by-macros-in-case-clauses
- OTP documentation in completion lists works again.
- Make sure includes in same dir as module is considered when finding things (NO TEST YET!)
- 782-dialyzer---add-checkbox-to-skip-plt-checks
- quicklaunch: autoinclude dependent projects
- 764-dialyzer---add-a-way-to-specify-plt-files-from-the-outside#last_comment
- allow unicode chars in comments
- 764-dialyzer---add-a-way-to-specify-plt-files-from-the-outside
- remove sdk feature from update site
- 756-navigation--external-include-files-are-not-found
- 757-erlang-model---command-line-option-to-turn-off-caching
- 756-navigation--external-include-files-are-not-found
- 734-make-modelutils-use-erlang-model--not-resource-model
- Clear project settings caches when project properties are changed (i.e. applied in the project property dialog). Move more from ModelUtils to model. Make sure Exceptions are propagated all the way to UI code (where they should be handled)
- 754-trying-to-open-an-erlang-debug-configuration-hangs-the-whole-eclipse
- 745-navigation---use-include-path-for-external-includes Fixed again...
- 723-outline---custom-filters
- 745-navigation---use-include-path-for-external-includes Fixed again...
- 732-navigation--merge-open-and-hover-code
- updated to be consisitent with wrangler-0.9.2 Emacs version.
- 727-dialyzer---better-gui-for-multiple-plt-files
- 682-dialyzer---multiple-plt-files (preliminary but well working GUI)

## 0.10.1 (20110216)

- 754-trying-to-open-an-erlang-debug-configuration-hangs-the-whole-eclipse

## 0.10.0 (20110209)

Minor version number updated because of internal API changes.

- 43-closing-project-should-dispose-internal-model
- modified markerutils to do less searching for resources
- 711-import-erlang-project---link-option-bad--remove-temporarily fixed so that it indeed copies...
- 630-navigator--create-links-to-otp
- 644-searching--show-an-icon-in-the-history (thanks, Vlad!)
- 731-erlang-navigator---external-should-expand-on-double-click
- 611-duplicated-and-weird-code-
- 729-indent--can-t-handle-binary-compehensions
- 717--externals--folder-in-navigator-shows-for-closed-projects
- 690-add-option-to-disable-content-assist Did it even better, with configurable delay
- https://github.com/erlide/erlide/issues/#issue/5 Fix problem to open files in erlang editor with other extension than erl, hrl or yrl
- 721-model---will-not-parse-file-in-non-erlang-project as noted by user on github https://github.com/erlide/erlide/issues/issue/5
- 724-searching--record-references-with-dot-messes-up-the-searching
- fix [#726](https://github.com/erlang/erlide_eclipse/issuesx/): strings larger than 64k are encoded as OtpErlangList, not OtpErlangString
- 682-dialyzer---multiple-plt-files (preliminary but well working GUI)
- eunit: create basic EUnit runner
- navigator: double click on project should open it
- eunit: no live feedback from runner
- dialyzer - specify functionality and use cases
- files with other extensions cannot be opened in erlang editor
- dialyzer - better gui for multiple plt files
- navigation---use-include-path-for-external-includes

## 0.9.5 (20110122)

- 702 externals: make external reference faster and less memory-hungry
- updated lambdaj to 2.3.2
- 669-dialyzer---add-plt-to-project-settings
- 659-navigator--double-click-on-project-should-open-it
- 652-searching--support-record-fields
- 224-variables-from-the--environment--tab-in-launch-config-are-not-set-in-the-started-process
- 692-project-properties-dialog-doesn-t-work
- 571-functions-defined-with-macros-confuses-the-model
- NPE in open when there are macro-defined function names
- 680-searching--wrong-display-in-result-header
- 618-dialyzer--remove-warnings-for-project use command framework
- 651-navigation--support-record-fields 653-hover--support-record-fields
- 625-debugger---should-present-terminate--continue--restart-when-reloading-code
- 386-debugger--modifying-code-disables-breakpoints
- 668 indentation confused by -spec directives
- 653 hover: support record fields
- 670 dialyzer: opens external files even if file is present in workspace
- 683 dialyzer: update plt files
- 691 internal launch config should not be saved on disk

## 0.9.4 (20101215)

- 678-navigation--navigating-to-erlide-modules-doesn-t-work
- fix NPE in ErlLogger
- 612-mark-occurrences---the-toggle-shows-up-over-java-files
- 647-mark-occurrences--off-by-default
- 654-searching--should-show-exported-functions-as-green-in-search-result
- 635-erlang-search--doesn-t-find-records-in-type-specs
- 516- changed erlide.log file location; also rewritten logger a bit.
- added toolbar item for "new module"
- [#661](https://github.com/erlang/erlide_eclipse/issues/661) nicer error message if erlang source is utf-8
- 666 moved "new" wizards to right menu

## 0.9.3 (20101125)

- 629-navigator--use-external-references-instead-of--external_files-
- 633-searching--use-external-references-instead-of--external_files-
- show visible message when ide backend can't be created
- fixed [#626](https://github.com/erlang/erlide_eclipse/issuesxed/): if default or erlide runtimes are deleted, set another one
- 617-dialyzer--remove-dialog
- Merge branch '646-let-erlide-plugins-use-local-beams' into pu
- 638-searching--can-t-handle-the--eunit--project

## 0.9.2 (20101108)

- merged wrangler r1627
- 628-navigation---open-(ctrl-click)-on-local-function-call-in-external-module-doesn
- Merge branch 'external-includes' into navigation
- Removed EXTERNAL_INCLUDES and EXTERNAL_MODULES flags, changed to two methods in model instead

## 0.9.1 (20101015)

* added several unit tests for our code
* general cleanup of the code
- 616-dialyzer-–-crash-on-binary-analysis-and-files-with-errors
- fixed [#614](https://github.com/erlang/erlide_eclipse/issuesxed/) open-module--dialog-shows-too-many-files
- disabled the wrangler "code inspection" menu, it isnt included in the release anyway

## 0.9.0 (20101004)

* new feature: tracing (ttb) integration (thanks to Piotr Dorobisz)
* added unit tests for our code
- Updated Wrangler to the latest version (r1573)
- 602-parser-can-t-handle-record-definitions-in-defines
- added jinterface from R14B
- 562-building-slow
- added ErlModelUtils.openMFA to open code in an editor
- fixed dialyzer exceptions
- 604-npe-blocks-the-ui
- erlide_proclist:initial_call could throw badmatch
- 608-dialyzer---navigate-to-external-includes-from-markers
git 71dd643 607-dialyzer---only-dialyze-on-selection

## 0.8.8 (20100903)

- 595-indentation---doesn-t-handle-binaries-with-macros-or-expressions
- provides folding for non-resource module editors 591-opening-erl-files-in-non-erlang-projects-fails
- 572-scanner-confused-by---v-in-macros
- 592-outline---compile-directive-has%C2%A0bad-presentation
- 572-scanner-confused-by---v-in-macros
- 512-templates-completion
- 585-mark-occurences--inconsistent-behaviour
- 519-hover---it-does-not-add-one-line-comments-as-documentation

## 0.8.7 (20100826)

- cleaned some TODO/FIXME
- updated testing plugins
- display version in pref page
- Merge branch 'bterl' into pu
- removed NPE
- updated CHANGES log
- updated readme
- wip
- small fixes
- updated coverage config
- small fixes
- no workspace import before tests
- enabled rpc test
- separated test from build
- removed unused code
- commented out dummy tests
- fixed builds with coverage
- wip junit launches
- added junit to bucky build

## 0.8.6 (20100823)

- now supporting Eclipse 3.5+ and Erlang R12+
- build process updated
- provides folding for non-resource module editors 591-opening-erl-files-in-non-erlang-projects-fails
- Fix of deadlock (eclipse hangs) with big files in erlang editor

## 0.8.2 (20100609)

- Merge branch '532-completion---confused-by-functions-defined-with-tuples' into shade_alpha
- Merge branch 'indentation' into shade_alpha
- Merge branch 'reconciler-still-broken' into shade_alpha
- Merge branch '567-and-316' into shade_alpha
- Merge branch 'folding_preferences' into shade_alpha
- 520-indentation-doesn-t-work-for-records-with-type-specs
- 575-new-module---template-error
- 316-navigate-to--macrofied--modules
- Handles hover, autocompletion and open on record given with macros (even if they are defined without macro).
- 464-renaming-a-module-leaves-a-scanner-process-running
- 469-editor---erlang-reconciler-still-broken
- 532-completion---confused-by-functions-defined-with-tuples
- build with dialyzer only if there are any modules in the project
- Added predefined macros (LINE, FILE, MODULE) for macro auto-completion and changed to case-insensitive matching (as in JDT)
- Removed some rarely used preferences for folding, and made sure it respects preferences on open.

## 0.8.1 (20100520)

- merge latest wrangler codebase update r1483
- fix [#384](https://github.com/erlang/erlide_eclipse/issuesx/) [#418](https://github.com/erlang/erlide_eclipse/issues/) [#540](https://github.com/erlangsues/erlide_eclipse/issues/) [#542](https://github.com/erlang/erlide_eclipse/issues/)
- Merge branch '549lide_eclipse-open-module-dialog---too-slow'
- 533-code-completion---record-fields-after---doesn-t-work

## List of user-visible changes in 0.8.0 (20100427)

- Merge branch 'open-typespec-in-records' into pu
- Merge remote branch 'vlad/e35' into pu
- Merge branch '547-launch--make-node-name-required-in-debug-mode' into next
- merge branch vlad/544-add--load-on-all-nodes--option-to-run-launches in pu
- merged branch "vlad/backend_shell_manager" into next
- Merge remote branch 'vlad/async_rpc' into pu
- Merge remote branch 'vlad/process_runner' into pu
- Merge remote branch 'vlad/bterl' into pu
- Merge remote branch 'vlad/console_stop' into next
- Merge remote branch vlad/remove_jrpc into next
- Merge remote branch 'vlad/remove_gunit' into next
- improved message when failing to restart backend

## 0.7.5 (20100329)

- fixed debugging on multiple nodes
- removed use of deprecated constructor
- fixed ext.point id reference
- removed reference to DebugUIPlugin
- removed illegal ErlStructureDiffViewer
- added license files for libraries
- removed epmd notification messages
- simplified some complicated functions
- simplify TermParser
- 533-code-completion---record-fields-after---doesn-t-work
- fix NPE when clicking @ in autocompletion info ctrl
- NPE on External Files
- fixed isAccessible
- fix bug in MarkerHelper
- updated plugin provider name
- erlang diff working and useful
- 531-indentation-in-lists-with-embedded-expressions-isn-t-quite-right

## 0.7.4 (20100304)

- added dialyzer support
- 222-errors-in-hrl-files
- 466 highlighting-of-macros-not-working
- 509-using-shortcut-to-toggle-comment-does-not-trigger-clearcase-checkout-dialog
- 526 run-debug-on-remote-node-doesn-t-work
- 518-templates---add-project-templates-from-external-xml-file
- 507-templates--unify-completion-templates-with--code-templates-
- 500-templates--popup-help-isn-t-formatted
- 504-templates--should-be-filtered-by-user-input
- 482-add-tab-for-src-and-include-dirs

## 0.7.3 (20100118)

- ignore errors when deleting beam files
- erlang templates, with syntax highlighted preview, indentation
- Merge remote branch 'jakob/490-External-Files-project-closed' into next
- use binary for noparse:initial_text parameter
- Fix timeout in light_scan_string, send bin instead of string
- Merge remote branch 'origin/487-configure-report-directory'
- Merge remote branch 'upstream/master'
- fix [#480](https://github.com/erlang/erlide_eclipse/issuesx/): import wizard doesn't descend in subdirectories
- send to console action, useful when debugging and testing
- Merge branch '257--type---navigate-from-spec-to-declared-type'
- fixed -opaque and external type refs
- add option to monitor ide backend
- improve backend launching: send environment; make node status local
- Merge branch '476-open-module-dialog--allow-inexact-matches'
- Merge branch '444-text-selected-randomly-in-editor'
- 444 - text selected randomly in editor
- 472-launch--extra-arguments-to-runtime-don-t-get-used
- Added OpenAndLinkWithEditorHelper.java for 3.4 compliance
- Merge branch '469-editor---erlang-reconciler-still-broken'
- set structure to true in reset (avoids erroneously reparse in compiler)
- CodeBundle now mirrors 'codepath' extensions moved project registration to Backend
- 468-split-erlang-code-so-that-compiler-backend-can-use-R11

## 0.7.0 (20091103)

- 362-random-erlang-crashes-on-Linux-with-exit-code-134
- delayed startup of backend until really needed
- 461-autocompletion-for-macros-records-with-quotes-doesn-t-work
- fix NPE if .erlang.cookie is empty
- builder: now twice as fast (parallel) and interruptible
- a NPE did hide connection problems
- added "report problems" on help menu
- documentation hover was too small at times
- 306-hover-documentation-with-non-edoc-comments-is-ugly
- added option to start internal backends with short name
- some console improvements (more to follow, it's not perfect yet!)
- no more ide console, use run/debug to start
- stdout is captured ([#221](https://github.com/erlangdout/erlide_eclipse/issues/))
- 147   	Debugger - interpret and un-interpret modules
- 243 	debugger: show the current stack even outside interpreted code
- 355 	debugger - terminated processes with stack look like they're running
- 390 	scanner and parser - should only update caches on save etc
- 402 	overriden 'int' module lacks functionality of the original one
- 426 	start backend on a standalone node.  Only start+console+codeloading are supported
- 441 	add "system info" functionality
- 451 	ErlFolder.getModules(IParent) is very slow
- 457 	builder: scan for TODO without a full scan

## 0.6.5 (r2877)

- The wrangler UI is completely revamped, offering much better user experience.
- 55-Erlang-Console-slowdown
- 57-Honoring--file%28%29-attribute
- 198--go-to-definition---if-no-match-was-found-based-on---args--use-first-matching-function-name \u2014 jakobc / detail
- 371-indent--error-after---MODULE \u2014 jakobc / detail
- 191-After-renaming-a-module-file--it-s-still-compiled-to-a-beam-with-the-old-name
- 229-Allow-----to-be-a-valid-source-directory-%28i-e-the-project-root%29
- 277-add-a--open-module--dialog-and-key-shortcut
- 	Ctrl-Shift-M and Alt-Ctrl-Shift-M
- 307-code-completion-and-otp-documentation-is-fetched-from-the-ide-backend--not-the-build-one
- 348-No-error-when--module-entry-doesn-t-match-filename
- 388-Change-key-binding-for--Reset-Outline-and-Cache-
- 382-annotation-hover-should-use-default-viewer--not-the-html-one
- 389-Navigates-to-External-Files-rather-than-internal-project
- 390-scanner-and-parser---should-only-update-caches-on-save-etc
- 393-add-Run-Debug-settings--working-directory----mnesia-dir-
- 395-Expose-the-setting-to-use-%E2%80%93sname-instead-of-%E2%80%93name-in-the-erl-runtime-config
- 397-problems-on-R11B5
- 403-view-module_info-data-on-property-page-for-module
- 406-ErlangEditor-getModule-can-return-null--not-all-call-sites-handle-that-case
- 408--record%28-MODULE--%7B%7D%29-shown-incorrectly-in-outline
- 414-console---ctrl-arrows-work-strangely
- nicer-looking console
- allow paths with unicode chars in them
- renamed external files project to External_Files (clearcase has problems with name with space in it)
- filter illegal characters from node name
- http://code.google.com/p/wranglerintegration/issues/detail?id=4 is fixed \u2014 oroszgy / detail
- http://code.google.com/p/wranglerintegration/issues/detail?id=3 is fixed \u2014 oroszgy / detail
- added option to start erlang with -noshell (on by default)
- launch config: made default to start the node if not found

## 0.6.2 (r2723)

- fixed exception in getErlideRuntime when installing for the first time
- duplicated code detection made to work in both Linux and Windows
- Wrangler code-base update
- 344-navigation---F3-goes-wrong-if-call-contains-fun----a-1-
- 343-debugger---breakpoints---group-on-type-gives-Others-(type-unspecified)
- work on 367-completion---better-layout-of-documentation
- 365-hover-over-non-folded-comment-shows-popup
- 369-command-to-reset-scanner-and-parser-caches
- 367-completion---better-layout-of-documentation
- Added null-test for scanner (can get null scanner from reconciler and builder when deleting resources)
- better looking edoc
- added support for setting debug_info and export_all options on Compiler
- 376-call-hierarchy--proof-of-concept: added a simplistic call hierarchy view, bound to Ctrl-Alt-H; more work required.


## 0.6.0 (r2571)

- fixed [erlide-Bugs-2771703 ] Can't start Erlide beyond 0.5.0
- 353-completion---X-rec---doesn-t-suggest-the-record-fields
- fix for [#2777621](https://github.com/erlang/erlide_eclipsex/issues/): also can't get above 0.5.0 working
- open should not find files not in the source path
- bug reported by daniel (indenting at eof in a gtt-file).
- 356-indent---cmd-i-crashes-if--Insert-spaces-for-tabs--is-selected
- 357-reconciler-can-hang-the-whole-eclipse
- work on [#2777890](https://github.com/erlang/erlide_eclipserk/issues/): Editor hangs
- raised reconnection delay


## 0.5.3 (r2525)

- Macros with lowercase didn't get hover, uppercase didn't complete
- 350-debugger---exited-process-sometimes-has-wrong-stack
- work on 349-Improve-console-view
- improved layout of ReportPreferencePage, hopefully better on OSX
- Major problems with scanner/noparser and synch, hopefully fixed:


## 0.5.2 (r2484)

- Bug fixed: wrong arity for function calls with funs
- bug: default ide runtime was not saved
- updated wrangler code base
- 315-console-doesn-t-work-with-R13
- raised rpc timeout value (fetching documentation is slow)
- 319-Sort-function-list-on-code-completion-after-module-
- 254-folding---preferences-don-t-work
- fixed bug: it didn't show function clauses, only functions w/o clauses (with 1 clause only)
- 310-model---put-externalIncludes-and-externalModules-and-path-vars-in-project-and-model
- 295-indentation---auto-linebreak-doesn-t-work-well
- 302-hover---it-should-use-the-fancy-new-eclipse-3-4-features
- 295-indentation---auto-linebreak-doesn-t-work-well
- 124-Editor-preference-page-doesn-t-work
- 295-indentation---auto-linebreak-doesn-t-work-well
- 202-Improved-code-completion-for-records
- 327-indentation--receive---after
- 322-Debugger---Keep-debug-context-even-if-the-process-is-killed-(works-in-OTP-debugger)
- 324-Debugger---Make-it-more-clear-when-a-breakpoint-has-been-hit
- 178-Debugger--group-erlang-processes-by-node
- 51-Syntax-colors
- 329-Unnecessary-parsing-of-hrl-files-on-doc-hovering
- 330-F3-doesn-t-find-definition-of-macro-if-name-begins-with-lowercase-letter
- 333-if-runtime-exists-but-is-broken--erlide-hangs
- 337-indent-problem
- 320-editor---bracket-insert-doesn-t-always-follow-prefs
- http://erlide.assembla.com/spaces/erlide/tickets/250-build-file-command
- 303-NPE-in-ErlangEditor-InformationDispatchAction
- 334-add-edoc-exporter

## 0.5.1 (r2365)

- Experimental refactoring support: Wrangler
- added automated problem report if backend dies with exit code != 0
- at startup, if no runtime is defined, search "obvious" locations and add all found runtimes. The newest will be set as default.
- ensuring that the ide backend exists (else print error messages as soon as possible)
- OTP R13-version of JInterface
- 275-Indent----end--and-indent-after-newline
- changed project properties to use a minimum required runtime version instead of a hardcoded runtime name
- 286-quick-outline---doesn-t-work-with-external-modules
- added Eclipse+Erlang launch configuration, to be used when debugging erlide itself
- 273-completion---doesn-t-find-some-builtins
- 289-code-completion---records-with---doesn-t-work
- 292-pressing-enter-at-the-end-of-a-line-sometimes-deletes-first-three-chars-on-next-line-
- 279-Text-Actions---shouldn-t-change-text-if-nothing-is-changed
- 287-debugger---better-display-of-binaries-and-lists
- 236-Debugger---show-records-as-records-in-Variables-view
- 269-debugger---better-display-of-stack-frames-and-breakpoints
- 290-code-completion---parameters-confused-by-binaries-in-clause-heads
- fixed serious bug (leaking threads in RpcDaemon)
- 299-indentation---the-atom-type-confuses-indentation
- 284-completion---context-sensitive
- 301-documentation-hover---doesn-t-work-in-extarnally-opened-files
- 300-quick-outline---still-doesn-t-work-with-external-modules
- 293-quick-outline--function-clauses-should-not-be-expanded-by-default
- 263-Show-non-edoc-comments-directly-from-code-on-hover-and-code-complete-
- 269-debugger---better-display-of-stack-frames-and-breakpoints
- 306-hover-documentation-with-non-edoc-comments-is-ugly
- 297-builder--backend-is-created-even-when-there-is-nothing-to-build
- 302-hover---it-should-use-the-fancy-new-eclipse-3-4-features
- 272-debugger---still-problems-with--debugged--tab-in-launch-config--dialog
- 304--line-macro-from-test_server-ruins-indentation
- 257--type---navigate-from-spec-to-declared-type
- 311-indent--error-with-macro-in--case-
- 312-outline---hide-type-and-spec-along-with-record-and-macro-defs




