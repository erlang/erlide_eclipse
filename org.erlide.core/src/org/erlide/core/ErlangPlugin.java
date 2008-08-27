/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Eric Merritt
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core;

import java.util.Enumeration;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Preferences;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.jinterface.ICodeBundle;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.ErlangProjectProperties;
import org.erlide.runtime.backend.BackendManager;
import org.osgi.framework.BundleContext;

/**
 * The main plugin class to be used in the desktop.
 * 
 * 
 * @author Eric Merritt [cyberlync at gmail dot com]
 * @author Vlad Dumitrescu [vladdu55 at gmail dot com]
 */
public class ErlangPlugin extends Plugin implements ICodeBundle {

	/**
	 * The plugin id
	 */
	public static final String PLUGIN_ID = "org.erlide.core";

	/**
	 * The builder id
	 */
	public static final String BUILDER_ID = PLUGIN_ID + ".erlbuilder";

	/**
	 * the nature id
	 */
	public static final String NATURE_ID = PLUGIN_ID + ".erlnature";

	/**
	 * The shared instance.
	 */
	private static ErlangPlugin plugin;

	/**
	 * Resource bundle.
	 */
	private ResourceBundle resourceBundle;

	/**
	 * The constructor.
	 */
	public ErlangPlugin() {
		super();
		plugin = this;
		try {
			resourceBundle = ResourceBundle
					.getBundle("org.erlide.core.ErlangPluginResources");
		} catch (final MissingResourceException x) {
			x.printStackTrace();
			resourceBundle = null;
		}
	}

	/**
	 * Returns the shared instance.
	 * 
	 * @return The plugin
	 */
	public static ErlangPlugin getDefault() {
		return plugin;
	}

	/**
	 * Returns the string from the plugin's resource bundle, or 'key' if not
	 * found.
	 * 
	 * @param key
	 *            The resource
	 * @return The identified string
	 */
	public static String getResourceString(String key) {
		final ResourceBundle bundle = ErlangPlugin.getDefault()
				.getResourceBundle();
		try {
			return (bundle != null) ? bundle.getString(key) : key;
		} catch (final MissingResourceException e) {
			return key;
		}
	}

	/**
	 * Returns the plugin's resource bundle,
	 * 
	 * @return The requested bundle
	 */
	public ResourceBundle getResourceBundle() {
		return resourceBundle;
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
			Map<String, String> attributes, IErlElement element) {
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
	public void configureErlangElementMarker(IMarker marker, IErlElement element)
			throws CoreException {
		// if (element instanceof IMember)
		// element = ((IMember) element).getClassFile();
		if (marker != null && element != null) {
			marker.setAttribute(ErlangCore.ATT_HANDLE_ID, "element handle id");
		}
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
	 *                           RECOGNIZED OPTIONS:
	 *                           COMPILER / Generating Source Debug Attribute
	 *                              When generated, this attribute will enable the debugger to present the
	 *                              corresponding source code.
	 *                               - option id:         &quot;org.erlide.core.erlang.compiler.debug.sourceFile&quot;
	 *                               - possible values:   { &quot;generate&quot;, &quot;do not generate&quot; }
	 *                               - default:           &quot;generate&quot;
	 * 
	 *                           COMPILER / Edoc Comment Support
	 *                              When this support is disabled, the compiler will ignore all Edoc problems options settings
	 *                              and will not report any Edoc problem. It will also not find any reference in Edoc comment and
	 *                              DOM AST Edoc node will be only a flat text instead of having structured tag elements.
	 *                               - option id:         &quot;org.erlide.core.erlang.compiler.doc.comment.support&quot;
	 *                               - possible values:   { &quot;enabled&quot;, &quot;disabled&quot; }
	 *                               - default:           &quot;enabled&quot;
	 * 
	 *                           COMPILER / Reporting Deprecation
	 *                              When enabled, the compiler will signal use of deprecated API either as an
	 *                              error or a warning.
	 *                               - option id:         &quot;org.erlide.core.erlang.compiler.problem.deprecation&quot;
	 *                               - possible values:   { &quot;error&quot;, &quot;warning&quot;, &quot;ignore&quot; }
	 *                               - default:           &quot;warning&quot;
	 * 
	 *                           COMPILER / Reporting Unused Local
	 *                              When enabled, the compiler will issue an error or a warning for unused local
	 *                              variables (that is, variables never read from)
	 *                               - option id:         &quot;org.erlide.core.erlang.compiler.problem.unusedLocal&quot;
	 *                               - possible values:   { &quot;error&quot;, &quot;warning&quot;, &quot;ignore&quot; }
	 *                               - default:           &quot;ignore&quot;
	 * 
	 *                           COMPILER / Reporting Unused Parameter
	 *                              When enabled, the compiler will issue an error or a warning for unused method
	 *                              parameters (that is, parameters never read from)
	 *                               - option id:         &quot;org.erlide.core.erlang.compiler.problem.unusedParameter&quot;
	 *                               - possible values:   { &quot;error&quot;, &quot;warning&quot;, &quot;ignore&quot; }
	 *                               - default:           &quot;ignore&quot;
	 * 
	 *                           COMPILER / Reporting Unused Private Functions
	 *                              When enabled, the compiler will issue an error or a warning whenever a private
	 *                              method or field is declared but never used within the same unit.
	 *                               - option id:         &quot;org.erlide.core.erlang.compiler.problem.unusedPrivateFunctions&quot;
	 *                               - possible values:   { &quot;error&quot;, &quot;warning&quot;, &quot;ignore&quot; }
	 *                               - default:           &quot;ignore&quot;
	 * 
	 *                           COMPILER / Reporting Local Variable Declaration Hiding another Variable
	 *                              When enabled, the compiler will issue an error or a warning whenever a local variable
	 *                              declaration is hiding some field or local variable (either locally, inherited or defined in enclosing type).
	 *                               - option id:         &quot;org.erlide.core.erlang.compiler.problem.localVariableHiding&quot;
	 *                               - possible values:   { &quot;error&quot;, &quot;warning&quot;, &quot;ignore&quot; }
	 *                               - default:           &quot;ignore&quot;
	 * 
	 *                           COMPILER / Reporting Invalid Edoc Comment
	 *                              This is the generic control for the severity of Edoc problems.
	 *                              When enabled, the compiler will issue an error or a warning for a problem in Edoc.
	 *                               - option id:         &quot;org.erlide.core.erlang.compiler.problem.invalidEdoc&quot;
	 *                               - possible values:   { &quot;error&quot;, &quot;warning&quot;, &quot;ignore&quot; }
	 *                               - default:           &quot;ignore&quot;
	 * 
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
	 * 
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
	 * 
	 *                           COMPILER / Reporting Missing Edoc Comments
	 *                              This is the generic control for the severity of missing Edoc comment problems.
	 *                              When enabled, the compiler will issue an error or a warning when Edoc comments are missing.
	 * &lt;br&gt;
	 *                              Note that this diagnosis can be enabled based on the visibility of the construct associated with the expected Edoc;
	 *                              also see the setting &quot;org.erlide.core.erlang.compiler.problem.missingEdocCommentsVisibility&quot;.
	 * &lt;br&gt;
	 *                               - option id:         &quot;org.erlide.core.erlang.compiler.problem.missingEdocComments&quot;
	 *                               - possible values:   { &quot;error&quot;, &quot;warning&quot;, &quot;ignore&quot; }
	 *                               - default:           &quot;ignore&quot;
	 * 
	 *                           COMPILER / Setting Compliance Level
	 *                              Select the compliance level for the compiler. In &quot;R9&quot; mode, source and target settings
	 *                              should not go beyond &quot;R9&quot; level.
	 *                               - option id:         &quot;org.erlide.core.erlang.compiler.compliance&quot;
	 *                               - possible values:   { &quot;R9&quot;, &quot;R10&quot; }
	 *                               - default:           &quot;R10&quot;
	 * 
	 *                           COMPILER / Maximum number of problems reported per compilation unit
	 *                              Specify the maximum number of problems reported on each compilation unit.
	 *                               - option id:         &quot;org.erlide.core.erlang.compiler.maxProblemPerUnit&quot;
	 *                               - possible values:    &quot;&lt;n&gt;&quot; where &lt;n&gt; is zero or a positive integer (if zero then all problems are reported).
	 *                               - default:           &quot;100&quot;
	 * 
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
	 * 
	 *                           COMPILER / Define the Automatic Task Priorities
	 *                              In parallel with the Automatic Task Tags, this list defines the priorities (high, normal or low)
	 *                              of the task markers issued by the compiler.
	 *                              If the default is specified, the priority of each task marker is &quot;NORMAL&quot;.
	 *                               - option id:         &quot;org.erlide.core.erlang.compiler.taskPriorities&quot;
	 *                               - possible values:   { &quot;&lt;priority&gt;[,&lt;priority&gt;]*&quot; } where &lt;priority&gt; is one of &quot;HIGH&quot;, &quot;NORMAL&quot; or &quot;LOW&quot;
	 *                               - default:           &quot;NORMAL,HIGH,NORMAL&quot;
	 * 
	 *                           COMPILER / Determine whether task tags are case-sensitive
	 *                              When enabled, task tags are considered in a case-sensitive way.
	 *                               - option id:         &quot;org.erlide.core.erlang.compiler.taskCaseSensitive&quot;
	 *                               - possible values:   { &quot;enabled&quot;, &quot;disabled&quot; }
	 *                               - default:           &quot;enabled&quot;
	 * 
	 *                           BUILDER / Abort if Invalid Classpath
	 *                              Allow to toggle the builder to abort if the classpath is invalid
	 *                               - option id:         &quot;org.erlide.core.erlang.builder.invalidClasspath&quot;
	 *                               - possible values:   { &quot;abort&quot;, &quot;ignore&quot; }
	 *                               - default:           &quot;abort&quot;
	 * 
	 *                           BUILDER / Cleaning Output Folder(s)
	 *                              Indicate whether the ErlangBuilder is allowed to clean the output folders
	 *                              when performing full build operations.
	 *                               - option id:         &quot;org.erlide.core.erlang.builder.cleanOutputFolder&quot;
	 *                               - possible values:   { &quot;clean&quot;, &quot;ignore&quot; }
	 *                               - default:           &quot;clean&quot;
	 * 
	 *                           BUILDER / Reporting Duplicate Resources
	 *                              Indicate the severity of the problem reported when more than one occurrence
	 *                              of a resource is to be copied into the output location.
	 *                               - option id:         &quot;org.erlide.core.erlang.builder.duplicateResourceTask&quot;
	 *                               - possible values:   { &quot;error&quot;, &quot;warning&quot; }
	 *                               - default:           &quot;warning&quot;
	 * 
	 *                           ErlangCORE / Computing Project Build Order
	 *                              Indicate whether ErlangCore should enforce the project build order to be based on
	 *                              the classpath prerequisite chain. When requesting to compute, this takes over
	 *                              the platform default order (based on project references).
	 *                               - option id:         &quot;org.erlide.core.erlang.computeErlangBuildOrder&quot;
	 *                               - possible values:   { &quot;compute&quot;, &quot;ignore&quot; }
	 *                               - default:           &quot;ignore&quot;
	 * 
	 *                           ErlangCORE / Reporting Incomplete Classpath
	 *                              Indicate the severity of the problem reported when an entry on the classpath does not exist,
	 *                              is not legite or is not visible (for example, a referenced project is closed).
	 *                               - option id:         &quot;org.erlide.core.erlang.incompleteClasspath&quot;
	 *                               - possible values:   { &quot;error&quot;, &quot;warning&quot;}
	 *                               - default:           &quot;error&quot;
	 * 
	 *                           ErlangCORE / Reporting Classpath Cycle
	 *                              Indicate the severity of the problem reported when a project is involved in a cycle.
	 *                               - option id:         &quot;org.erlide.core.erlang.circularClasspath&quot;
	 *                               - possible values:   { &quot;error&quot;, &quot;warning&quot; }
	 *                               - default:           &quot;error&quot;
	 * 
	 *                           ErlangCORE / Reporting Incompatible ERTS Level for Required Binaries
	 *                              Indicate the severity of the problem reported when a project prerequisites another project
	 *                              or library with an incompatible target ERTS level (e.g. project targeting R7 vm, but compiled against R10 libraries).
	 *                               - option id:         &quot;org.erlide.core.erlang.incompatibleJDKLevel&quot;
	 *                               - possible values:   { &quot;error&quot;, &quot;warning&quot;, &quot;ignore&quot; }
	 *                               - default:           &quot;ignore&quot;
	 * 
	 * 
	 * 
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
		final Preferences preferences = getDefault().getPluginPreferences();
		final HashSet<String> optionNames = ErlangCore.getModelManager()
				.getOptionNames();

		// initialize preferences to their default
		final Iterator<String> iterator = optionNames.iterator();
		while (iterator.hasNext()) {
			final String propertyName = iterator.next();
			defaultOptions.put(propertyName, preferences
					.getDefaultString(propertyName));
		}
		// get encoding through resource plugin
		defaultOptions.put(ErlangCore.CORE_ENCODING, getEncoding());

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
	 * Helper method for returning one option value only. Equivalent to
	 * <code>(String)ErlangCore.getOptions().get(optionName)</code> Note that it
	 * may answer <code>null</code> if this option does not exist.
	 * <p>
	 * For a complete description of the configurable options, see
	 * <code>getDefaultOptions</code>.
	 * </p>
	 * 
	 * @param optionName
	 *            the name of an option
	 * @return the String value of a given option
	 * @see ErlangCore#getDefaultOptions()
	 */
	public static String getOption(String optionName) {

		if (ErlangCore.CORE_ENCODING.equals(optionName)) {
			return getEncoding();
		}
		final String propertyName = optionName;
		if (ErlangCore.getModelManager().getOptionNames()
				.contains(propertyName)) {
			final Preferences preferences = getDefault().getPluginPreferences();
			return preferences.getString(propertyName).trim();
		}
		return null;
	}

	/**
	 * Returns the table of the current options. Initially, all options have
	 * their default values, and this method returns a table that includes all
	 * known options.
	 * <p>
	 * For a complete description of the configurable options, see
	 * <code>getDefaultOptions</code>.
	 * </p>
	 * 
	 * @return table of current settings of all options (key type:
	 *         <code>String</code>; value type: <code>String</code>)
	 * @see ErlangCore#getDefaultOptions()
	 */
	public static Hashtable<String, String> getOptions() {

		final Hashtable<String, String> options = new Hashtable<String, String>(
				10);

		// see #initializeDefaultPluginPreferences() for changing default
		// settings
		final Plugin thePlugin = getDefault();
		if (thePlugin != null) {
			final Preferences preferences = getDefault().getPluginPreferences();
			final HashSet<String> optionNames = ErlangCore.getModelManager()
					.getOptionNames();

			// initialize preferences to their default
			final Iterator<String> iterator = optionNames.iterator();
			while (iterator.hasNext()) {
				final String propertyName = iterator.next();
				options.put(propertyName, preferences
						.getDefaultString(propertyName));
			}
			// get preferences not set to their default
			final String[] propertyNames = preferences.propertyNames();
			for (final String propertyName : propertyNames) {
				final String value = preferences.getString(propertyName).trim();
				if (optionNames.contains(propertyName)) {
					options.put(propertyName, value);
				}
			}
			// get encoding through resource plugin
			options.put(ErlangCore.CORE_ENCODING, getEncoding());
		}
		return options;
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
	public static void run(IWorkspaceRunnable action, IProgressMonitor monitor)
			throws CoreException {
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
	public static void run(IWorkspaceRunnable action, ISchedulingRule rule,
			IProgressMonitor monitor) throws CoreException {
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

	/**
	 * Sets the current table of options. All and only the options explicitly
	 * included in the given table are remembered; all previous option settings
	 * are forgotten, including ones not explicitly mentioned.
	 * <p>
	 * For a complete description of the configurable options, see
	 * <code>getDefaultOptions</code>.
	 * </p>
	 * 
	 * @param newOptions
	 *            the new options (key type: <code>String</code>; value type:
	 *            <code>String</code>), or <code>null</code> to reset all
	 *            options to their default values
	 * @see ErlangCore#getDefaultOptions()
	 */
	public static void setOptions(Hashtable<String, String> newOptions) {

		// see #initializeDefaultPluginPreferences() for changing default
		// settings
		final Preferences preferences = getDefault().getPluginPreferences();

		if (newOptions == null) {
			newOptions = ErlangPlugin.getDefaultOptions();
		}
		final Enumeration<String> keys = newOptions.keys();
		while (keys.hasMoreElements()) {
			final String key = keys.nextElement();
			if (!ErlangCore.getModelManager().getOptionNames().contains(key)) {
				continue; // unrecognized option
			}
			if (key.equals(ErlangCore.CORE_ENCODING)) {
				continue; // skipped, contributed by resource prefs
			}
			final String value = newOptions.get(key);
			preferences.setValue(key, value);
		}

		// persist options
		getDefault().savePluginPreferences();
	}

	/*
	 * (non-Edoc) Shutdown the ErlangCore plug-in. <p> De-registers the
	 * ErlModelManager as a resource changed listener and save participant. <p>
	 * 
	 * @see org.eclipse.core.runtime.Plugin#stop(BundleContext)
	 */
	@Override
	public void stop(BundleContext context) throws Exception {
		try {
			try {
				// savePluginPreferences();
				// final IWorkspace workspace = ResourcesPlugin.getWorkspace();
				// workspace.removeResourceChangeListener(ErlModelManager.
				// getDefault().deltaState);
				// workspace.removeSaveParticipant(this);

				ErlangCore.getModelManager().shutdown();
			} finally {
				BackendManager.getDefault().removePlugin(this);

				// ensure we call super.stop as the last thing
				super.stop(context);
			}
		} catch (final Exception e) {
			e.printStackTrace();
		}
	}

	/*
	 * (non-Edoc) Startup the ErlangCore plug-in. <p> Registers the
	 * ErlModelManager as a resource changed listener and save participant.
	 * Starts the background indexing, and restore saved classpath variable
	 * values. <p> @throws Exception
	 * 
	 * @see org.eclipse.core.runtime.Plugin#start(BundleContext)
	 */
	@Override
	public void start(BundleContext context) throws Exception {
		ErlLogger.debug("Starting CORE");
		super.start(context);

		BackendManager.getDefault().register(this);

		registerOpenProjects();

		// final IErlModelManager manager = ErlangCore.getModelManager();
		// try {
		// // request state folder creation (workaround 19885)
		// getDefault().getStateLocation();
		//
		// // retrieve variable values
		// getDefault().getPluginPreferences().addPropertyChangeListener(
		// new ErlModelManager.PluginPreferencesListener());
		//
		// // final IWorkspace workspace = ResourcesPlugin.getWorkspace();
		// // workspace.addResourceChangeListener(manager.deltaState,
		// // IResourceChangeEvent.PRE_BUILD
		// // | IResourceChangeEvent.POST_BUILD |
		// // IResourceChangeEvent.POST_CHANGE
		// // | IResourceChangeEvent.PRE_DELETE |
		// // IResourceChangeEvent.PRE_CLOSE);
		//
		// // process deltas since last activated in indexer thread so that
		// // indexes are up-to-date.
		// // see https://bugs.eclipse.org/bugs/show_bug.cgi?id=38658
		// final Job processSavedState = new Job(Util
		// .bind("savedState.jobName")) { //$NON-NLS-1$
		//
		// @Override
		// protected IStatus run(IProgressMonitor monitor) {
		// // try
		// // {
		// // // add save participant and process delta atomically
		// // // see
		// // // https://bugs.eclipse.org/bugs/show_bug.cgi?id=59937
		// // workspace.run(new IWorkspaceRunnable() {
		// //
		// // public void run(IProgressMonitor progress) throws
		// // CoreException
		// // {
		// // ISavedState savedState = workspace.addSaveParticipant(
		// // getDefault(), manager);
		// // if (savedState != null)
		// // {
		// // // the event type coming from the saved
		// // // state is always POST_AUTO_BUILD
		// // // force it to be POST_CHANGE so that the
		// // // delta processor can handle it
		// // //
		// // manager.deltaState.getDeltaProcessor().overridenEventType
		// // // = IResourceChangeEvent.POST_CHANGE;
		// // // savedState
		// // // .processResourceChangeEvents(manager.deltaState);
		// // }
		// // }
		// // }, monitor);
		// // } catch (CoreException e)
		// // {
		// // return e.getStatus();
		// // }
		// return Status.OK_STATUS;
		// }
		// };
		// processSavedState.setSystem(true);
		// processSavedState.setPriority(Job.SHORT); // process asap
		// processSavedState.schedule();
		// } catch (final RuntimeException e) {
		// manager.shutdown();
		// throw e;
		// }
		ErlLogger.debug("Started CORE");
	}

	private static void registerOpenProjects() {
		final IWorkspace root = ResourcesPlugin.getWorkspace();
		final IProject[] projects = root.getRoot().getProjects();

		// String[] nameOfProjects = new String[projects.length];

		for (IProject element : projects) {
			try {
				if (element.isOpen()
						&& element.hasNature(ErlangPlugin.NATURE_ID)) {
					final ErlangProjectProperties prefs = new ErlangProjectProperties(
							element);
					final String path = element.getLocation().append(
							prefs.getOutputDir()).toString();
					BackendManager.getDefault().getIdeBackend().addPath(
							prefs.getUsePathZ(), path);
				}
			} catch (final CoreException e) {
				e.printStackTrace();
			}
		}
	}

	public static void log(IStatus status) {
		getDefault().getLog().log(status);
	}

	public static void logErrorMessage(String message) {
		log(new Status(IStatus.ERROR, PLUGIN_ID,
				IErlangStatusConstants.INTERNAL_ERROR, message, null));
	}

	public static void logErrorStatus(String message, IStatus status) {
		if (status == null) {
			logErrorMessage(message);
			return;
		}
		final MultiStatus multi = new MultiStatus(PLUGIN_ID,
				IErlangStatusConstants.INTERNAL_ERROR, message, null);
		multi.add(status);
		log(multi);
	}

	public static void log(Throwable e) {
		log(new Status(IStatus.ERROR, PLUGIN_ID,
				IErlangStatusConstants.INTERNAL_ERROR, "Erlide internal error",
				e));
	}

	public static void initializeAfterLoad(IProgressMonitor monitor) {
		final IWorkspaceRunnable runnable = new IWorkspaceRunnable() {

			public void run(IProgressMonitor progressMonitor)
					throws CoreException {
				IProject[] projects = null;
				// projects = model.getJavaProjects();
				IWorkspace root = ResourcesPlugin.getWorkspace();
				projects = root.getRoot().getProjects();

				if (projects != null) {
					for (IProject project : projects) {
						try {
							if (project.hasNature(ErlangPlugin.NATURE_ID)) {
								project.touch(progressMonitor);
							}
						} catch (CoreException e) {
							// could not touch this project: ignore
						}
					}
				}
			}
		};
		try {
			ResourcesPlugin.getWorkspace().run(runnable, monitor);
		} catch (final CoreException e) {
			// could not touch all projects
		}

	}

	public static void log(String msg, Throwable thr) {
		final String id = PLUGIN_ID;
		final Status status = new Status(IStatus.ERROR, id, IStatus.OK, msg,
				thr);
		getDefault().getLog().log(status);
	}

	public static void debug(String message) {
		if (getDefault().isDebugging()) {
			ErlLogger.debug(message);
		}
	}

	public void start() {
	}

}
