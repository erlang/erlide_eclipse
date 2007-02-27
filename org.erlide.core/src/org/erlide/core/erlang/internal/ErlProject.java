/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.core.erlang.internal;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.ICommand;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
// import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Preferences;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.reconciler.DirtyRegion;
import org.erlide.core.ErlangPlugin;
// import org.erlide.core.ErlangProjectProperties;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlModelStatus;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModelMarker;
import org.erlide.core.erlang.IErlModelStatus;
import org.erlide.core.erlang.IErlModelStatusConstants;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.util.ISuffixConstants;
import org.erlide.core.erlang.util.Util;

/**
 * Handle for an Erlang Project.
 * 
 * <p>
 * A Erlang Project internally maintains a devpath that corresponds to the
 * project's classpath. The classpath may include source folders from the
 * current project; jars in the current project, other projects, and the local
 * file system; and binary folders (output location) of other projects. The
 * Erlang Model presents source elements corresponding to output .class files in
 * other projects, and thus uses the devpath rather than the classpath (which is
 * really a compilation path). The devpath mimics the classpath, except has
 * source folder entries in place of output locations in external projects.
 * 
 * <p>
 * Each ErlProject has a NameLookup facility that locates elements on by name,
 * based on the devpath.
 * 
 * @see IErlProject
 */
public class ErlProject extends Openable implements IErlProject,
		ISuffixConstants {

	/**
	 * Whether the underlying file system is case sensitive.
	 */
	protected static final boolean IS_CASE_SENSITIVE = !new File("Temp").equals(new File("temp")); //$NON-NLS-1$ //$NON-NLS-2$

	/**
	 * An empty array of strings indicating that a project doesn't have any
	 * prerequesite projects.
	 */
	protected static final String[] NO_PREREQUISITES = new String[0];

	/**
	 * The platform project this <code>IErlProject</code> is based on
	 */
	protected IProject fProject;

	/**
	 * A array with all the non-Erlang resources contained by this
	 * PackageFragment
	 */
	private IResource[] nonErlangResources;

	/**
	 * Name of file containing project classpath
	 */
	public static final String CODEPATH_FILENAME = ".codepath"; //$NON-NLS-1$

	/**
	 * Name of file containing custom project preferences
	 */
	public static final String PREF_FILENAME = ".eprefs"; //$NON-NLS-1$

	private static final String CUSTOM_DEFAULT_OPTION_VALUE = "#\r\n\r#custom-non-empty-default-value#\r\n\r#"; //$NON-NLS-1$

	/**
	 * Constructor needed for <code>IProject.getNature()</code> and
	 * <code>IProject.addNature()</code>.
	 * 
	 * @see #setProject(IProject)
	 */
	public ErlProject() {
		super(null, null);
		nonErlangResources = null;
	}

	ErlProject(IProject project, ErlElement parent) {
		super(parent, project.getName());
		fProject = project;
		nonErlangResources = null;
	}

	/**
	 * Adds a builder to the build spec for the given project.
	 */
	protected void addToBuildSpec(String builderID) throws CoreException {
		final IProjectDescription description = fProject.getDescription();
		final int erlangCommandIndex = getErlangCommandIndex(description
				.getBuildSpec());

		if (erlangCommandIndex == -1) {
			// Add a Erlang command to the build spec
			final ICommand command = description.newCommand();
			command.setBuilderName(builderID);
			setErlangCommand(description, command);
		}
	}

	/**
	 * @see Openable
	 */
	@Override
	protected boolean buildStructure(IProgressMonitor pm,
			IResource underlyingResource, IDocument doc, DirtyRegion dirtyRegion)
			throws ErlModelException {
		// check whether the Erlang project can be opened
		if (!underlyingResource.isAccessible()) {
			throw newNotPresentException();
		}
		System.out.println("--- " + getProject().getName() + "? "
				+ fChildren.length);
		if (fChildren != null && fChildren.length != 0) {
			System.out.println("--- !");
			return true;
		}

		// final IWorkspace workspace = ResourcesPlugin.getWorkspace();
		// final IWorkspaceRoot wRoot = workspace.getRoot();

		final List<IErlModule> modules = new ArrayList<IErlModule>(10);
		try {
			// TODO use project preferences to find which dirs are source dirs
			// final ErlangProjectProperties pprop = new
			// ErlangProjectProperties(getProject());

			final IContainer c = (IContainer) underlyingResource;
			final IResource[] elems = c.members();

			// System.out.println("--- >>> " + this.getElementName());

			// TODO should do this lazily!
			buildStructure(elems, modules);

		} catch (final CoreException e) {
			e.printStackTrace();
		}
		fChildren = new ErlElement[modules.size()];
		System.arraycopy(modules.toArray(), 0, fChildren, 0, modules.size());
		System.out.println("---YY " + fChildren.length);

		return true;
	}

	private void buildStructure(IResource[] elems, List<IErlModule> modules) {
		for (IResource element : elems) {
			// System.out.println("---< " + elems[fi].getName());
			if (element instanceof IFolder) {
				final IFolder ff = (IFolder) element;
				try {
					buildStructure(ff.members(), modules);
				} catch (final CoreException e) {
					; // ignore
				}
			} else if (element instanceof IFile) {
				final IFile f = (IFile) element;
				final String ext = f.getFileExtension();
				if (ext != null && (ext.equals("erl") || ext.equals("hrl"))) {
					// int l = f.getFileExtension().length();
					// String mn = f.getName().substring(0, f.getName().length()
					// - l - 1);

					// System.out.println("--- l " + f.getName());
					final IErlModule m = ErlangCore.getModelManager()
							.createModuleFrom(f, this);
					modules.add(m);
				}
			}
		}

	}

	@Override
	protected void closing(Object info) {
		super.closing(info);
	}

	/**
	 * Computes the collection of modules (local ones) and set it on the given
	 * info.
	 * 
	 * @param info
	 *            ErlProjectElementInfo
	 * @throws ErlModelException
	 */
	public void computeChildren(ErlProject info) throws ErlModelException {
		// TODO fix this
		info.setNonErlangResources(null);
		info.setChildren(null);
	}

	/**
	 * Configure the project with Erlang nature.
	 */
	public void configure() throws CoreException {
		// register Erlang builder
		addToBuildSpec(ErlangPlugin.BUILDER_ID);
	}

	/*
	 * Returns whether the given resource is accessible through the children or
	 * the non-Erlang resources of this project. Returns true if the resource is
	 * not in the project. Assumes that the resource is a folder or a file.
	 */
	public boolean contains(IResource resource) {
		//
		// IClasspathEntry[] classpath;
		// IPath output;
		// try
		// {
		// classpath = getResolvedClasspath(true/* ignoreUnresolvedEntry */,
		// false/* don't generateMarkerOnError */, false/*
		// * don't
		// * returnResolutionInProgress
		// */);
		// output = getOutputLocation();
		// }
		// catch (ErlModelException e)
		// {
		// return false;
		// }
		//
		// IPath fullPath = resource.getFullPath();
		// IPath innerMostOutput = output.isPrefixOf(fullPath) ? output : null;
		// IClasspathEntry innerMostEntry = null;
		// for (int j = 0, cpLength = classpath.length; j < cpLength; j++)
		// {
		// IClasspathEntry entry = classpath[j];
		//
		// IPath entryPath = entry.getPath();
		// if ((innerMostEntry == null || innerMostEntry.getPath().isPrefixOf(
		// entryPath))
		// && entryPath.isPrefixOf(fullPath))
		// {
		// innerMostEntry = entry;
		// }
		// IPath entryOutput = classpath[j].getOutputLocation();
		// if (entryOutput != null && entryOutput.isPrefixOf(fullPath))
		// {
		// innerMostOutput = entryOutput;
		// }
		// }
		// if (innerMostEntry != null)
		// {
		// // special case prj==src and nested output location
		// if (innerMostOutput != null && innerMostOutput.segmentCount() > 1 //
		// output
		// // isn't
		// // project
		// && innerMostEntry.getPath().segmentCount() == 1)
		// { // 1 segment must be project name
		// return false;
		// }
		// if (resource instanceof IFolder)
		// {
		// // folders are always included in src/lib entries
		// return true;
		// }
		// switch (innerMostEntry.getEntryKind())
		// {
		// case IClasspathEntry.CPE_SOURCE :
		// // .class files are not visible in source folders
		// return !org.eclipse.jdt.internal.compiler.util.Util
		// .isClassFileName(fullPath.lastSegment());
		// case IClasspathEntry.CPE_LIBRARY :
		// // .Erlang files are not visible in library folders
		// return !org.eclipse.jdt.internal.compiler.util.Util
		// .isErlangFileName(fullPath.lastSegment());
		// }
		// }
		// if (innerMostOutput != null)
		// {
		// return false;
		// }
		return true;
	}

	/**
	 * TODO: Record a new marker denoting a classpath problem
	 */
	void createCodeProblemMarker(IErlModelStatus status) {
		/*
		 * final IMarker marker = null; int severity; String[] arguments = new
		 * String[0]; final boolean isCycleProblem = false,
		 * isClasspathFileFormatProblem = false; switch (status.getCode()) {
		 * 
		 * case IErlModelStatusConstants.INCOMPATIBLE_ERTS_LEVEL: final String
		 * setting = getOption( ErlangCore.CORE_INCOMPATIBLE_ERTS_LEVEL, true);
		 * if (ErlangCore.ERROR.equals(setting)) { severity =
		 * IMarker.SEVERITY_ERROR; } else if
		 * (ErlangCore.WARNING.equals(setting)) { severity =
		 * IMarker.SEVERITY_WARNING; } else { return; // setting == IGNORE }
		 * break;
		 * 
		 * default: final IPath path = status.getPath(); if (path != null) {
		 * arguments = new String[] { path.toString() }; } if
		 * (ErlangCore.ERROR.equals(getOption(
		 * ErlangCore.CORE_INCOMPLETE_CLASSPATH, true))) { severity =
		 * IMarker.SEVERITY_ERROR; } else { severity = IMarker.SEVERITY_WARNING; }
		 * break; }
		 */
	}

	/**
	 * /** Removes the Erlang nature from the project.
	 */
	public void deconfigure() throws CoreException {
		// unregister Erlang builder
		removeFromBuildSpec(ErlangPlugin.BUILDER_ID);
	}

	/**
	 * Returns a default output location. This is the project bin folder
	 */
	protected IPath defaultOutputLocation() {
		return fProject.getFullPath().append("ebin"); //$NON-NLS-1$
	}

	/**
	 * Returns true if this handle represents the same Erlang project as the
	 * given handle. Two handles represent the same project if they are
	 * identical or if they represent a project with the same underlying
	 * resource and occurrence counts.
	 * 
	 * @see ErlElement#equals(Object)
	 */
	@Override
	public boolean equals(Object o) {

		if (this == o) {
			return true;
		}

		if (!(o instanceof ErlProject)) {
			return false;
		}

		final ErlProject other = (ErlProject) o;
		return fProject.equals(other.getProject())
				&& fOccurrenceCount == other.fOccurrenceCount;
	}

	@Override
	public boolean exists() {
		return ErlangCore.hasErlangNature(fProject);
	}

	/**
	 * @see IErlProject
	 */
	public IErlElement findElement(IPath path) throws ErlModelException {

		if (path == null || path.isAbsolute()) {
			throw new ErlModelException(new ErlModelStatus(
					IErlModelStatusConstants.INVALID_PATH, path));
		}
		/*
		 * TODO: realizate findElement(IPath path) final String extension =
		 * path.getFileExtension(); if
		 * (extension.equalsIgnoreCase(EXTENSION_ERL) ||
		 * extension.equalsIgnoreCase(EXTENSION_BEAM)) { final IPath packagePath =
		 * path.removeLastSegments(1); final String packageName =
		 * packagePath.toString().replace( IPath.SEPARATOR, '.'); String
		 * typeName = path.lastSegment(); typeName = typeName.substring(0,
		 * typeName.length() - extension.length() - 1); String qualifiedName =
		 * null; if (packageName.length() > 0) { qualifiedName = packageName +
		 * "." + typeName; //$NON-NLS-1$ } else { qualifiedName = typeName; } }
		 * else { // unsupported extension return null; }
		 */
		return null;
	}

	/**
	 * Remove all markers denoting classpath problems
	 */
	protected void flushCodepathProblemMarkers(boolean flushCycleMarkers,
			boolean flushCodepathFormatMarkers) {
		try {
			if (fProject.isAccessible()) {
				final IMarker[] markers = fProject.findMarkers(
						IErlModelMarker.BUILDPATH_PROBLEM_MARKER, false,
						IResource.DEPTH_ZERO);
				for (final IMarker marker : markers) {
					if (flushCycleMarkers && flushCodepathFormatMarkers) {
						marker.delete();
					}
				}
			}
		} catch (final CoreException e) {
			// could not flush markers: not much we can do
			if (ErlModelManager.verbose) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * @see IErlElement
	 */
	public String getElementType() {
		return PROJECT;
	}

	/**
	 * Returns the <code>char</code> that marks the start of this handles
	 * contribution to a memento.
	 */
	protected char getHandleMementoDelimiter() {

		return EM_PROJECT;
	}

	/**
	 * Find the specific Erlang command amongst the given build spec and return
	 * its index or -1 if not found.
	 */
	private int getErlangCommandIndex(ICommand[] buildSpec) {

		for (int i = 0; i < buildSpec.length; ++i) {
			if (buildSpec[i].getBuilderName().equals(ErlangPlugin.BUILDER_ID)) {
				return i;
			}
		}
		return -1;
	}

	/**
	 * Returns an array of non-Erlang resources contained in the receiver.
	 */
	public IResource[] getNonErlangResources() throws ErlModelException {

		return getNonErlangResources(this);
	}

	/**
	 * @see org.erlide.core.erlang.IErlProject#getOption(String, boolean)
	 */
	public String getOption(String optionName, boolean inheritErlangCoreOptions) {

		final String propertyName = optionName;
		if (ErlangCore.getModelManager().getOptionNames()
				.contains(propertyName)) {
			final Preferences preferences = getPreferences();
			if (preferences == null || preferences.isDefault(propertyName)) {
				return inheritErlangCoreOptions ? ErlangPlugin
						.getOption(propertyName) : null;
			}
			return preferences.getString(propertyName).trim();
		}
		return null;
	}

	/**
	 * @see org.erlide.core.erlang.IErlProject#getOptions(boolean)
	 */
	public Map<String, String> getOptions(boolean inheritErlangCoreOptions) {

		// initialize to the defaults from ErlangCore options pool
		final Map<String, String> options = inheritErlangCoreOptions ? ErlangPlugin
				.getOptions()
				: new Hashtable<String, String>(5);

		final Preferences preferences = getPreferences();
		if (preferences == null) {
			return options; // cannot do better (non-Erlang project)
		}
		final HashSet optionNames = ErlangCore.getModelManager()
				.getOptionNames();

		// project cannot hold custom preferences set to their default, as it
		// uses CUSTOM_DEFAULT_OPTION_VALUE

		// get custom preferences not set to their default
		final String[] propertyNames = preferences.propertyNames();
		for (final String propertyName : propertyNames) {
			final String value = preferences.getString(propertyName).trim();
			if (optionNames.contains(propertyName)) {
				options.put(propertyName, value);
			}
		}

		return options;
	}

	/**
	 * @see IErlProject
	 */
	public IPath getOutputLocation() throws ErlModelException {
		// Do not create marker but log problems while getting output location
		return this.getOutputLocation(false, true);
	}

	/**
	 * @param createMarkers
	 *            boolean
	 * @param logProblems
	 *            boolean
	 * @return IPath
	 * @throws ErlModelException
	 */
	public IPath getOutputLocation(boolean createMarkers, boolean logProblems)
			throws ErlModelException {

		final ErlModelManager.PerProjectInfo perProjectInfo = getPerProjectInfo();
		IPath outputLocation = perProjectInfo.outputLocation;
		if (outputLocation != null) {
			return outputLocation;
		}

		// force to read classpath - will position output location as well
		// this.getRawClasspath(createMarkers, logProblems);
		outputLocation = perProjectInfo.outputLocation;
		if (outputLocation == null) {
			return defaultOutputLocation();
		}
		return outputLocation;
	}

	public ErlModelManager.PerProjectInfo getPerProjectInfo()
			throws ErlModelException {
		return ErlangCore.getModelManager().getPerProjectInfoCheckExistence(
				fProject);
	}

	/**
	 * @see IErlProject#getProject()
	 */
	public IProject getProject() {
		return fProject;
	}

	/**
	 * Returns the project custom preference pool. Project preferences may
	 * include custom encoding.
	 * 
	 * @return Preferences
	 */
	protected Preferences getPreferences() {
		if (!ErlangCore.hasErlangNature(fProject)) {
			return null;
		}
		final ErlModelManager.PerProjectInfo perProjectInfo = ErlModelManager
				.getDefault().getPerProjectInfo(fProject, true);
		Preferences preferences = perProjectInfo.preferences;
		if (preferences != null) {
			return preferences;
		}
		preferences = loadPreferences();
		if (preferences == null) {
			preferences = new Preferences();
		}
		perProjectInfo.preferences = preferences;
		return preferences;
	}

	/**
	 * @see IErlProject#getRequiredProjectNames()
	 */
	public String[] getRequiredProjectNames() throws ErlModelException {
		return null;

		// return this.projectPrerequisites(getResolvedClasspath(true, false,
		// false));
	}

	/**
	 * @see IErlElement
	 */
	public IResource getResource() {
		return fProject;
	}

	/**
	 * @see IErlElement
	 */
	@Override
	public IResource getUnderlyingResource() throws ErlModelException {
		if (!exists()) {
			throw newNotPresentException();
		}
		return fProject;
	}

	/**
	 * @see IErlProject
	 */
	public boolean hasBuildState() {
		return ErlangCore.getModelManager().getLastBuiltState(fProject,
				null) != null;
	}

	@Override
	public int hashCode() {
		if (fProject == null) {
			return super.hashCode();
		}
		return fProject.hashCode();
	}

	private IPath getPluginWorkingLocation() {
		return fProject.getWorkingLocation(ErlangPlugin.PLUGIN_ID);
	}

	/*
	 * load preferences from a shareable format (VCM-wise)
	 */
	public Preferences loadPreferences() {

		final Preferences preferences = new Preferences();

		// File prefFile =
		// this.project.getLocation().append(PREF_FILENAME).toFile();
		final IPath projectMetaLocation = getPluginWorkingLocation();
		if (projectMetaLocation != null) {
			final File prefFile = projectMetaLocation.append(PREF_FILENAME)
					.toFile();
			if (prefFile.exists()) { // load preferences from file
				InputStream in = null;
				try {
					in = new BufferedInputStream(new FileInputStream(prefFile));
					preferences.load(in);
					return preferences;
				} catch (final IOException e) { // problems loading preference
					// store
					// - quietly ignore
				} finally {
					if (in != null) {
						try {
							in.close();
						} catch (final IOException e) { // ignore problems with
							// close
						}
					}
				}
			}
		}
		return null;
	}

	/**
	 * Removes the given builder from the build spec for the given project.
	 */
	protected void removeFromBuildSpec(String builderID) throws CoreException {

		final IProjectDescription description = fProject.getDescription();
		final ICommand[] commands = description.getBuildSpec();
		for (int i = 0; i < commands.length; ++i) {
			if (commands[i].getBuilderName().equals(builderID)) {
				final ICommand[] newCommands = new ICommand[commands.length - 1];
				System.arraycopy(commands, 0, newCommands, 0, i);
				System.arraycopy(commands, i + 1, newCommands, i,
						commands.length - i - 1);
				description.setBuildSpec(newCommands);
				fProject.setDescription(description, null);
				return;
			}
		}
	}

	/**
	 * Answers an PLUGIN_ID which is used to distinguish project/entries during
	 * package fragment root computations
	 * 
	 * @return String
	 */
	public String rootID() {
		return "[PRJ]" + fProject.getFullPath(); //$NON-NLS-1$
	}

	/**
	 * Save project custom preferences to shareable file (.jprefs)
	 */
	private void savePreferences(Preferences preferences) {

		if (!ErlangCore.hasErlangNature(fProject)) {
			return; // ignore
		}

		if (preferences == null
				|| (!preferences.needsSaving() && preferences.propertyNames().length != 0)) {
			// nothing to save
			return;
		}

		// preferences need to be saved
		// the preferences file is located in the plug-in's state area
		// at a well-known name (.jprefs)
		// File prefFile =
		// this.project.getLocation().append(PREF_FILENAME).toFile();
		final File prefFile = getPluginWorkingLocation().append(PREF_FILENAME)
				.toFile();
		if (preferences.propertyNames().length == 0) {
			// there are no preference settings
			// rather than write an empty file, just delete any existing file
			if (prefFile.exists()) {
				prefFile.delete(); // don't worry if delete unsuccessful
			}
			return;
		}

		// write file, overwriting an existing one
		OutputStream out = null;
		try {
			// do it as carefully as we know how so that we don't lose/mangle
			// the setting in times of stress
			out = new BufferedOutputStream(new FileOutputStream(prefFile));
			preferences.store(out, null);
		} catch (final IOException e) { // problems saving preference store -
			// quietly ignore
		} finally {
			if (out != null) {
				try {
					out.close();
				} catch (final IOException e) { // ignore problems with close
				}
			}
		}
	}

	/**
	 * Update the Erlang command in the build spec (replace existing one if
	 * present, add one first if none).
	 */
	private void setErlangCommand(IProjectDescription description,
			ICommand newCommand) throws CoreException {
		final ICommand[] oldBuildSpec = description.getBuildSpec();
		final int oldErlangCommandIndex = getErlangCommandIndex(oldBuildSpec);
		ICommand[] newCommands;

		if (oldErlangCommandIndex == -1) {
			// Add a Erlang build spec before other builders (1FWJK7I)
			newCommands = new ICommand[oldBuildSpec.length + 1];
			System.arraycopy(oldBuildSpec, 0, newCommands, 1,
					oldBuildSpec.length);
			newCommands[0] = newCommand;
		} else {
			oldBuildSpec[oldErlangCommandIndex] = newCommand;
			newCommands = oldBuildSpec;
		}

		// Commit the spec change into the project
		description.setBuildSpec(newCommands);
		fProject.setDescription(description, null);
	}

	/**
	 * @see org.erlide.core.erlang.IErlProject#setOption(java.lang.String,
	 *      java.lang.String)
	 */
	public void setOption(String optionName, String optionValue) {
		if (!ErlangCore.getModelManager().getOptionNames().contains(optionName)) {
			return; // unrecognized option
		}
		final Preferences preferences = getPreferences();
		preferences.setDefault(optionName, CUSTOM_DEFAULT_OPTION_VALUE);
		preferences.setValue(optionName, optionValue);
		savePreferences(preferences);
	}

	/**
	 * @see org.erlide.core.erlang.IErlProject#setOptions(Map)
	 */
	public void setOptions(Map newOptions) {

		final Preferences preferences = getPreferences();
		if (newOptions != null) {
			final Iterator keys = newOptions.keySet().iterator();
			while (keys.hasNext()) {
				final String key = (String) keys.next();
				if (!ErlangCore.getModelManager().getOptionNames()
						.contains(key)) {
					continue; // unrecognized option
				}
				// no filtering for encoding (custom encoding for project is
				// allowed)
				final String value = (String) newOptions.get(key);
				preferences.setDefault(key, CUSTOM_DEFAULT_OPTION_VALUE);
				preferences.setValue(key, value);
			}
		}

		// reset to default all options not in new map
		final String[] pNames = preferences.propertyNames();
		final int ln = pNames.length;
		for (int i = 0; i < ln; i++) {
			final String key = pNames[i];
			if (newOptions == null || !newOptions.containsKey(key)) {
				preferences.setToDefault(key); // set default => remove from
				// preferences table
			}
		}

		// persist options
		savePreferences(preferences);
	}

	/**
	 * @see IErlProject
	 */
	public void setOutputLocation(IPath path, IProgressMonitor monitor)
			throws ErlModelException {
		if (path == null) {
			throw new IllegalArgumentException(Util.bind("path.nullPath")); //$NON-NLS-1$
		}
		if (path.equals(getOutputLocation())) {
			return;
		}
		// this.setRawClasspath(SetClasspathOperation.ReuseClasspath, path,
		// monitor);
	}

	/*
	 * Set cached preferences, no preference file is saved, only info is updated
	 */
	public void setPreferences(Preferences preferences) {
		if (!ErlangCore.hasErlangNature(fProject)) {
			return; // ignore
		}
		final ErlModelManager.PerProjectInfo perProjectInfo = ErlModelManager
				.getDefault().getPerProjectInfo(fProject, true);
		perProjectInfo.preferences = preferences;
	}

	/**
	 * Sets the underlying kernel project of this Erlang project, and fills in
	 * its parent and name. Called by IProject.getNature().
	 * 
	 * @see IProjectNature#setProject(IProject)
	 */
	public void setProject(IProject project) {
		fProject = project;
		fParent = ErlangCore.getModel();
		fName = project.getName();
	}

	public IErlModule[] getModules() throws ErlModelException {
		return (IErlModule[]) fChildren;
	}

	/**
	 * Returns a canonicalized path from the given external path. Note that the
	 * return path contains the same number of segments and it contains a device
	 * only if the given path contained one.
	 * 
	 * @param externalPath
	 *            IPath
	 * @see java.io.File for the definition of a canonicalized path
	 * @return IPath
	 */
	public static IPath canonicalizedPath(IPath externalPath) {

		if (externalPath == null) {
			return null;
		}

		if (IS_CASE_SENSITIVE) {
			return externalPath;
		}

		// if not external path, return original path
		final IWorkspace workspace = ResourcesPlugin.getWorkspace();
		if (workspace == null) {
			return externalPath; // protection during shutdown (30487)
		}
		if (workspace.getRoot().findMember(externalPath) != null) {
			return externalPath;
		}

		IPath canonicalPath = null;
		try {
			canonicalPath = new Path(new File(externalPath.toOSString())
					.getCanonicalPath());
		} catch (final IOException e) {
			// default to original path
			return externalPath;
		}

		IPath result;
		final int canonicalLength = canonicalPath.segmentCount();
		if (canonicalLength == 0) {
			// the java.io.File canonicalization failed
			return externalPath;
		} else if (externalPath.isAbsolute()) {
			result = canonicalPath;
		} else {
			// if path is relative, remove the first segments that were added by
			// the java.io.File canonicalization
			// e.g. 'lib/classes.zip' was converted to
			// 'd:/myfolder/lib/classes.zip'
			final int externalLength = externalPath.segmentCount();
			if (canonicalLength >= externalLength) {
				result = canonicalPath.removeFirstSegments(canonicalLength
						- externalLength);
			} else {
				return externalPath;
			}
		}

		// keep device only if it was specified (this is because
		// File.getCanonicalPath() converts '/lib/classed.zip' to
		// 'd:/lib/classes/zip')
		if (externalPath.getDevice() == null) {
			result = result.setDevice(null);
		}
		return result;
	}

	/**
	 * Returns an array of non-Erlang resources contained in the receiver.
	 */
	IResource[] getNonErlangResources(ErlProject project) {

		if (nonErlangResources == null) {
			nonErlangResources = null;
		}
		return nonErlangResources;
	}

	/**
	 * Set the fNonErlangResources to res value
	 */
	void setNonErlangResources(IResource[] resources) {

		nonErlangResources = resources;
	}

	public IErlModule getModule(String name) throws ErlModelException {
		if (fChildren == null || fChildren.length == 0) {
			open(null);
		}
		for (IErlElement element : fChildren) {
			final IErlModule m = (IErlModule) element;
			if (m != null && m.getElementName().equals(name)) {
				return m;
			}
		}
		return null;
	}

	public boolean isVisibleInOutline() {
		return false;
	}

}