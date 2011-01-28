/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation
 *******************************************************************************/
package org.erlide.core.erlang;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.preferences.DefaultScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.internal.ErlModelManager;
import org.erlide.core.erlang.internal.ErlModuleMap;
import org.erlide.jinterface.backend.RuntimeInfo;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.RuntimeInfoManager;

/**
 * <p>
 * The single instance of this class can be accessed from any plug-in declaring
 * the Erlang model plug-in as a prerequisite via
 * <code>ErlangCore.getErlangCore()</code>. The Erlang model plug-in will be
 * activated automatically if not already active.
 * </p>
 */
public final class ErlangCore {

    /**
     * The identifier for the Erlang model (value
     * <code>"org.erlide.core.erlang.erlangmodel"</code>).
     */
    public static final String MODEL_ID = ErlangPlugin.PLUGIN_ID
            + ".erlangmodel"; //$NON-NLS-1$

    /**
     * Name of the handle id attribute in a Erlang marker.
     */
    public static final String ATT_HANDLE_ID = "org.erlide.core.erlang.internal.ErlModelManager.handleId"; //$NON-NLS-1$

    public static final IErlModelManager getModelManager() {
        return ErlModelManager.getDefault();
    }

    public static final IErlModel getModel() {
        return getModelManager().getErlangModel();
    }

    public static final IErlModuleMap getModuleMap() {
        return ErlModuleMap.getDefault();
    }

    public static final RuntimeInfoManager getRuntimeInfoManager() {
        return RuntimeInfoManager.getDefault();
    }

    public static final BackendManager getBackendManager() {
        return BackendManager.getDefault();
    }

    public static IOldErlangProjectProperties getProjectProperties(
            final IProject project) {
        return getModelManager().getErlangModel()
                .getErlangProject(project.getName()).getProperties();
    }

    /**
     * If runtime is not set, try to locate one. The first one found as below is
     * set as default. All "obvious" runtimes found are stored.
     * <ul>
     * <li>A system property <code>erlide.runtime</code> can be set to point to
     * a location.</li>
     * <li>A preference in the default scope
     * <code>org.erlide.core/default_runtime</code> can be set to point to a
     * location.</li>
     * <li>Look for existing Erlang runtimes in a few obvious places and install
     * them, choosing a suitable one as default.</li>
     * </ul>
     * 
     */
    public static void initializeRuntimesList() {
        if (getRuntimeInfoManager().getDefaultRuntime() != null) {
            return;
        }
        final String[] locations = {
                System.getProperty("erlide.runtime"),
                new DefaultScope().getNode("org.erlide.core").get(
                        "default_runtime", null), "c:/program files",
                "c:/programs", "c:/", "c:/apps",
                System.getProperty("user.home"), "/usr", "/usr/local",
                "/usr/local/lib", "/Library/Frameworks/erlang/Versions" };
        for (final String loc : locations) {
            final Collection<File> roots = findRuntime(loc);
            for (final File root : roots) {
                final RuntimeInfo rt = new RuntimeInfo();
                rt.setOtpHome(root.getPath());
                rt.setName(root.getName());
                final IWorkspaceRoot wroot = ResourcesPlugin.getWorkspace()
                        .getRoot();
                final String location = wroot.getLocation().toPortableString();
                rt.setWorkingDir(location);
                getRuntimeInfoManager().addRuntime(rt);
            }
        }
        final List<RuntimeInfo> list = new ArrayList<RuntimeInfo>(
                getRuntimeInfoManager().getRuntimes());
        Collections.sort(list, new Comparator<RuntimeInfo>() {
            public int compare(final RuntimeInfo o1, final RuntimeInfo o2) {
                final int x = o2.getVersion().compareTo(o1.getVersion());
                if (x != 0) {
                    return x;
                }
                return o2.getName().compareTo(o1.getName());
            }
        });
        if (list.size() > 0) {
            getRuntimeInfoManager().setDefaultRuntime(list.get(0).getName());
            getRuntimeInfoManager().setErlideRuntime(
                    getRuntimeInfoManager().getDefaultRuntime());
        }
    }

    private static Collection<File> findRuntime(final String loc) {
        final Collection<File> result = new ArrayList<File>();
        if (loc == null) {
            return result;
        }
        final File folder = new File(loc);
        if (!folder.exists()) {
            return result;
        }
        final File[] candidates = folder.listFiles(new FileFilter() {
            public boolean accept(final File pathname) {
                return pathname.isDirectory()
                        && (pathname.getName().startsWith("erl")
                                || pathname.getName().startsWith("Erl") || pathname
                                .getName().startsWith("R"));
            }
        });
        for (final File f : candidates) {
            if (RuntimeInfo.validateLocation(f.getPath())) {
                result.add(f);
            }
        }
        return result;
    }

    /**
     * Returns a table of all known configurable options with their default
     * values. These options allow to configure the behaviour of the underlying
     * components. The client may safely use the result as a template that they
     * can modify and then pass to <code>setOptions</code>.
     * 
     * Helper constants have been defined on ErlangCore for each of the option
     * PLUGIN_ID and their possible constant values.
     * 
     * Note: more options might be added in further releases.
     * 
     * <pre>
     * RECOGNIZED OPTIONS:
     * COMPILER / Generating Source Debug Attribute
     *    When generated, this attribute will enable the debugger to present the
     *    corresponding source code.
     *     - option id:         &quot;org.erlide.core.erlang.compiler.debug.sourceFile&quot;
     *     - possible values:   { &quot;generate&quot;, &quot;do not generate&quot; }
     *     - default:           &quot;generate&quot;
     * COMPILER / Edoc Comment Support
     *   When this support is disabled, the compiler will ignore all Edoc problems options settings
     *   and will not report any Edoc problem. It will also not find any reference in Edoc comment and
     *   DOM AST Edoc node will be only a flat text instead of having structured tag elements.
     *     - option id:         &quot;org.erlide.core.erlang.compiler.doc.comment.support&quot;
     *     - possible values:   { &quot;enabled&quot;, &quot;disabled&quot; }
     *     - default:           &quot;enabled&quot;
     *                           COMPILER / Reporting Deprecation
     *                              When enabled, the compiler will signal use of deprecated API either as an
     *                              error or a warning.
     *                               - option id:         &quot;org.erlide.core.erlang.compiler.problem.deprecation&quot;
     *                               - possible values:   { &quot;error&quot;, &quot;warning&quot;, &quot;ignore&quot; }
     *                               - default:           &quot;warning&quot;
     *                           COMPILER / Reporting Unused Local
     *                              When enabled, the compiler will issue an error or a warning for unused local
     *                              variables (that is, variables never read from)
     *                               - option id:         &quot;org.erlide.core.erlang.compiler.problem.unusedLocal&quot;
     *                               - possible values:   { &quot;error&quot;, &quot;warning&quot;, &quot;ignore&quot; }
     *                               - default:           &quot;ignore&quot;
     *                           COMPILER / Reporting Unused Parameter
     *                              When enabled, the compiler will issue an error or a warning for unused method
     *                              parameters (that is, parameters never read from)
     *                               - option id:         &quot;org.erlide.core.erlang.compiler.problem.unusedParameter&quot;
     *                               - possible values:   { &quot;error&quot;, &quot;warning&quot;, &quot;ignore&quot; }
     *                               - default:           &quot;ignore&quot;
     *                           COMPILER / Reporting Unused Private Functions
     *                              When enabled, the compiler will issue an error or a warning whenever a private
     *                              method or field is declared but never used within the same unit.
     *                               - option id:         &quot;org.erlide.core.erlang.compiler.problem.unusedPrivateFunctions&quot;
     *                               - possible values:   { &quot;error&quot;, &quot;warning&quot;, &quot;ignore&quot; }
     *                               - default:           &quot;ignore&quot;
     *                           COMPILER / Reporting Local Variable Declaration Hiding another Variable
     *                              When enabled, the compiler will issue an error or a warning whenever a local variable
     *                              declaration is hiding some field or local variable (either locally, inherited or defined in enclosing type).
     *                               - option id:         &quot;org.erlide.core.erlang.compiler.problem.localVariableHiding&quot;
     *                               - possible values:   { &quot;error&quot;, &quot;warning&quot;, &quot;ignore&quot; }
     *                               - default:           &quot;ignore&quot;
     *                           COMPILER / Reporting Invalid Edoc Comment
     *                              This is the generic control for the severity of Edoc problems.
     *                              When enabled, the compiler will issue an error or a warning for a problem in Edoc.
     *                               - option id:         &quot;org.erlide.core.erlang.compiler.problem.invalidEdoc&quot;
     *                               - possible values:   { &quot;error&quot;, &quot;warning&quot;, &quot;ignore&quot; }
     *                               - default:           &quot;ignore&quot;
     *                           COMPILER / Reporting Invalid Edoc Tags
     *                              When enabled, the compiler will signal unbound or unexpected reference tags in Edoc.
     *                              A 'throws' tag referencing an undeclared exception would be considered as unexpected.
     * &lt;br&gt;
     *                              Note that this diagnosis can be enabled based on the visibility of the construct associated with the Edoc;
     *                              also see the setting &quot;org.erlide.core.erlang.compiler.problem.invalidEdocTagsVisibility&quot;.
     * &lt;br&gt;
     *                              The severity of the problem is controlled with option &quot;org.erlide.core.erlang.compiler.problem.invalidEdoc&quot;.
     *                               - option id:         &quot;org.erlide.core.erlang.compiler.problem.invalidEdocTags&quot;
     *                               - possible values:   { &quot;disabled&quot;, &quot;enabled&quot; }
     *                               - default:           &quot;enabled&quot;
     *                           COMPILER / Reporting Missing Edoc Tags
     *                              This is the generic control for the severity of Edoc missing tag problems.
     *                              When enabled, the compiler will issue an error or a warning when tags are missing in Edoc comments.
     * &lt;br&gt;
     *                              Note that this diagnosis can be enabled based on the visibility of the construct associated with the Edoc;
     *                              also see the setting &quot;org.erlide.core.erlang.compiler.problem.missingEdocTagsVisibility&quot;.
     * &lt;br&gt;
     *                               - option id:         &quot;org.erlide.core.erlang.compiler.problem.missingEdocTags&quot;
     *                               - possible values:   { &quot;error&quot;, &quot;warning&quot;, &quot;ignore&quot; }
     *                               - default:           &quot;ignore&quot;
     * COMPILER / Reporting Missing Edoc Comments
     *   This is the generic control for the severity of missing Edoc comment problems.
     *   When enabled, the compiler will issue an error or a warning when Edoc comments are missing.
     * &lt;br&gt;
     *   Note that this diagnosis can be enabled based on the visibility of the construct associated with the expected Edoc;
     *   also see the setting &quot;org.erlide.core.erlang.compiler.problem.missingEdocCommentsVisibility&quot;.
     * &lt;br&gt;
     *     - option id:         &quot;org.erlide.core.erlang.compiler.problem.missingEdocComments&quot;
     *     - possible values:   { &quot;error&quot;, &quot;warning&quot;, &quot;ignore&quot; }
     *     - default:           &quot;ignore&quot;
     *                           COMPILER / Setting Compliance Level
     *                              Select the compliance level for the compiler. In &quot;R9&quot; mode, source and target settings
     *                              should not go beyond &quot;R9&quot; level.
     *                               - option id:         &quot;org.erlide.core.erlang.compiler.compliance&quot;
     *                               - possible values:   { &quot;R9&quot;, &quot;R10&quot; }
     *                               - default:           &quot;R10&quot;
     *                           COMPILER / Maximum number of problems reported per compilation unit
     *                              Specify the maximum number of problems reported on each compilation unit.
     *                               - option id:         &quot;org.erlide.core.erlang.compiler.maxProblemPerUnit&quot;
     *                               - possible values:    &quot;&lt;n&gt;&quot; where &lt;n&gt; is zero or a positive integer (if zero then all problems are reported).
     *                               - default:           &quot;100&quot;
     *                           COMPILER / Define the Automatic Task Tags
     *                              When the tag list is not empty, the compiler will issue a task marker whenever it encounters
     *                              one of the corresponding tag inside any comment in Erlang source code.
     *                              Generated task messages will include the tag, and range until the next line separator or comment ending.
     *                              Note that tasks messages are trimmed. If a tag is starting with a letter or digit, then it cannot be leaded by
     *                              another letter or digit to be recognized (&quot;fooToDo&quot; will not be recognized as a task for tag &quot;ToDo&quot;, but &quot;foo#ToDo&quot;
     *                              will be detected for either tag &quot;ToDo&quot; or &quot;#ToDo&quot;). Respectively, a tag ending with a letter or digit cannot be followed
     *                              by a letter or digit to be recognized (&quot;ToDofoo&quot; will not be recognized as a task for tag &quot;ToDo&quot;, but &quot;ToDo:foo&quot; will
     *                              be detected either for tag &quot;ToDo&quot; or &quot;ToDo:&quot;).
     *                               - option id:         &quot;org.erlide.core.erlang.compiler.taskTags&quot;
     *                               - possible values:   { &quot;&lt;tag&gt;[,&lt;tag&gt;]*&quot; } where &lt;tag&gt; is a String without any wild-card or leading/trailing spaces
     *                               - default:           &quot;TODO,FIXME,XXX&quot;
     *                           COMPILER / Define the Automatic Task Priorities
     *                              In parallel with the Automatic Task Tags, this list defines the priorities (high, normal or low)
     *                              of the task markers issued by the compiler.
     *                              If the default is specified, the priority of each task marker is &quot;NORMAL&quot;.
     *                               - option id:         &quot;org.erlide.core.erlang.compiler.taskPriorities&quot;
     *                               - possible values:   { &quot;&lt;priority&gt;[,&lt;priority&gt;]*&quot; } where &lt;priority&gt; is one of &quot;HIGH&quot;, &quot;NORMAL&quot; or &quot;LOW&quot;
     *                               - default:           &quot;NORMAL,HIGH,NORMAL&quot;
     *                           COMPILER / Determine whether task tags are case-sensitive
     *                              When enabled, task tags are considered in a case-sensitive way.
     *                               - option id:         &quot;org.erlide.core.erlang.compiler.taskCaseSensitive&quot;
     *                               - possible values:   { &quot;enabled&quot;, &quot;disabled&quot; }
     *                               - default:           &quot;enabled&quot;
     *                           BUILDER / Abort if Invalid Classpath
     *                              Allow to toggle the builder to abort if the classpath is invalid
     *                               - option id:         &quot;org.erlide.core.erlang.builder.invalidClasspath&quot;
     *                               - possible values:   { &quot;abort&quot;, &quot;ignore&quot; }
     *                               - default:           &quot;abort&quot;
     *                           BUILDER / Cleaning Output Folder(s)
     *                              Indicate whether the ErlangBuilder is allowed to clean the output folders
     *                              when performing full build operations.
     *                               - option id:         &quot;org.erlide.core.erlang.builder.cleanOutputFolder&quot;
     *                               - possible values:   { &quot;clean&quot;, &quot;ignore&quot; }
     *                               - default:           &quot;clean&quot;
     *                           BUILDER / Reporting Duplicate Resources
     *                              Indicate the severity of the problem reported when more than one occurrence
     *                              of a resource is to be copied into the output location.
     *                               - option id:         &quot;org.erlide.core.erlang.builder.duplicateResourceTask&quot;
     *                               - possible values:   { &quot;error&quot;, &quot;warning&quot; }
     *                               - default:           &quot;warning&quot;
     *                           ErlangCORE / Computing Project Build Order
     *                              Indicate whether ErlangCore should enforce the project build order to be based on
     *                              the classpath prerequisite chain. When requesting to compute, this takes over
     *                              the platform default order (based on project references).
     *                               - option id:         &quot;org.erlide.core.erlang.computeErlangBuildOrder&quot;
     *                               - possible values:   { &quot;compute&quot;, &quot;ignore&quot; }
     *                               - default:           &quot;ignore&quot;
     *                           ErlangCORE / Reporting Incomplete Classpath
     *                              Indicate the severity of the problem reported when an entry on the classpath does not exist,
     *                              is not legite or is not visible (for example, a referenced project is closed).
     *                               - option id:         &quot;org.erlide.core.erlang.incompleteClasspath&quot;
     *                               - possible values:   { &quot;error&quot;, &quot;warning&quot;}
     *                               - default:           &quot;error&quot;
     *                           ErlangCORE / Reporting Classpath Cycle
     *                              Indicate the severity of the problem reported when a project is involved in a cycle.
     *                               - option id:         &quot;org.erlide.core.erlang.circularClasspath&quot;
     *                               - possible values:   { &quot;error&quot;, &quot;warning&quot; }
     *                               - default:           &quot;error&quot;
     * ErlangCORE / Reporting Incompatible ERTS Level for Required Binaries
     *   Indicate the severity of the problem reported when a project prerequisites another project
     *   or library with an incompatible target ERTS level (e.g. project targeting R7 vm, but compiled against R10 libraries).
     *     - option id:         &quot;org.erlide.core.erlang.incompatibleJDKLevel&quot;
     *     - possible values:   { &quot;error&quot;, &quot;warning&quot;, &quot;ignore&quot; }
     *     - default:           &quot;ignore&quot;
     * </pre>
     * 
     * @return a mutable table containing the default settings of all known
     *         options (key type: <code>String</code>; value type:
     *         <code>String</code>)
     * @see #setOptions(Hashtable)
     */
    public static Hashtable<String, String> getDefaultOptions() {

        final Hashtable<String, String> defaultOptions = new Hashtable<String, String>(
                10);

        // see #initializeDefaultPluginPreferences() for changing default
        // settings
        final IEclipsePreferences preferences = new DefaultScope()
                .getNode(ErlangPlugin.PLUGIN_ID);
        final HashSet<String> optionNames = getModelManager().getOptionNames();

        // initialize preferences to their default
        final Iterator<String> iterator = optionNames.iterator();
        while (iterator.hasNext()) {
            final String propertyName = iterator.next();
            defaultOptions.put(propertyName, preferences.get(propertyName, ""));
        }
        // get encoding through resource plugin
        defaultOptions.put(ErlangCoreOptions.CORE_ENCODING, getEncoding());

        return defaultOptions;
    }

    /**
     * Returns the workspace root default charset encoding.
     * 
     * @return the name of the default charset encoding for workspace root.
     * @see IContainer#getDefaultCharset()
     * @see ResourcesPlugin#getEncoding()
     */
    public static String getEncoding() {
        // Verify that workspace is not shutting down (see bug
        // https://bugs.eclipse.org/bugs/show_bug.cgi?id=60687)
        final IWorkspace workspace = ResourcesPlugin.getWorkspace();
        if (workspace != null) {
            try {
                return workspace.getRoot().getDefaultCharset();
            } catch (final CoreException e) {
                // fails silently and return plugin global encoding if core
                // exception occurs
            }
        }
        return ResourcesPlugin.getEncoding();
    }

    /**
     * Configures the given marker attribute map for the given Erlang element.
     * Used for markers, which denote a Erlang element rather than a resource.
     * 
     * @param attributes
     *            the mutable marker attribute map (key type:
     *            <code>String</code>, value type: <code>String</code>)
     * @param element
     *            the Erlang element for which the marker needs to be configured
     */
    public static void addErlangElementMarkerAttributes(
            final Map<String, String> attributes, final IErlElement element) {
        // if (element instanceof IMember)
        // element = ((IMember) element).getClassFile();
        if (attributes != null && element != null) {
            attributes.put(ErlangCore.ATT_HANDLE_ID, "element handle id");
        }
    }

    /**
     * Configures the given marker for the given Erlang element. Used for
     * markers, which denote a Erlang element rather than a resource.
     * 
     * @param marker
     *            the marker to be configured
     * @param element
     *            the Erlang element for which the marker needs to be configured
     * @throws CoreException
     *             if the <code>IMarker.setAttribute</code> on the marker fails
     */
    public static void configureErlangElementMarker(final IMarker marker,
            final IErlElement element) throws CoreException {
        // if (element instanceof IMember)
        // element = ((IMember) element).getClassFile();
        if (marker != null && element != null) {
            marker.setAttribute(ErlangCore.ATT_HANDLE_ID, "element handle id");
        }
    }

    /**
     * Runs the given action as an atomic Erlang model operation.
     * <p>
     * After running a method that modifies Erlang elements, registered
     * listeners receive after-the-fact notification of what just transpired, in
     * the form of a element changed event. This method allows clients to call a
     * number of methods that modify Erlang elements and only have element
     * changed event notifications reported at the end of the entire batch.
     * </p>
     * <p>
     * If this method is called outside the dynamic scope of another such call,
     * this method runs the action and then reports a single element changed
     * event describing the net effect of all changes done to Erlang elements by
     * the action.
     * </p>
     * <p>
     * If this method is called in the dynamic scope of another such call, this
     * method simply runs the action.
     * </p>
     * 
     * @param action
     *            the action to perform
     * @param monitor
     *            a progress monitor, or <code>null</code> if progress reporting
     *            and cancellation are not desired
     * @throws CoreException
     *             if the operation failed.
     */
    public static void run(final IWorkspaceRunnable action,
            final IProgressMonitor monitor) throws CoreException {
        run(action, ResourcesPlugin.getWorkspace().getRoot(), monitor);
    }

    /**
     * Runs the given action as an atomic Erlang model operation.
     * <p>
     * After running a method that modifies Erlang elements, registered
     * listeners receive after-the-fact notification of what just transpired, in
     * the form of a element changed event. This method allows clients to call a
     * number of methods that modify Erlang elements and only have element
     * changed event notifications reported at the end of the entire batch.
     * </p>
     * <p>
     * If this method is called outside the dynamic scope of another such call,
     * this method runs the action and then reports a single element changed
     * event describing the net effect of all changes done to Erlang elements by
     * the action.
     * </p>
     * <p>
     * If this method is called in the dynamic scope of another such call, this
     * method simply runs the action.
     * </p>
     * <p>
     * The supplied scheduling rule is used to determine whether this operation
     * can be run simultaneously with workspace changes in other threads. See
     * <code>IWorkspace.run(...)</code> for more details.
     * </p>
     * 
     * @param action
     *            the action to perform
     * @param rule
     *            the scheduling rule to use when running this operation, or
     *            <code>null</code> if there are no scheduling restrictions for
     *            this operation.
     * @param monitor
     *            a progress monitor, or <code>null</code> if progress reporting
     *            and cancellation are not desired
     * @throws CoreException
     *             if the operation failed.
     */
    public static void run(final IWorkspaceRunnable action,
            final ISchedulingRule rule, final IProgressMonitor monitor)
            throws CoreException {
        final IWorkspace workspace = ResourcesPlugin.getWorkspace();
        if (workspace.isTreeLocked()) {
            // new BatchOperation(action).run(monitor);
        } else {
            // use IWorkspace.run(...) to ensure that a build will be done in
            // autobuild mode
            // workspace.run(new BatchOperation(action), rule,
            // IWorkspace.AVOID_UPDATE, monitor);
        }
    }

    private ErlangCore() {
    }
}
