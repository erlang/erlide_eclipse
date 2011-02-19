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

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.ICommand;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.erlide.backend.Backend;
import org.erlide.backend.BackendException;
import org.erlide.backend.rpc.RpcCallSite;
import org.erlide.backend.runtime.RuntimeInfo;
import org.erlide.backend.runtime.RuntimeVersion;
import org.erlide.backend.util.PreferencesUtils;
import org.erlide.backend.util.StringUtils;
import org.erlide.backend.util.Util;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlElementVisitor;
import org.erlide.core.erlang.IErlExternal;
import org.erlide.core.erlang.IErlFolder;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModelManager;
import org.erlide.core.erlang.IErlModelMarker;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IOldErlangProjectProperties;
import org.erlide.core.erlang.IOpenable;
import org.erlide.core.erlang.IParent;
import org.erlide.core.erlang.internal.ErlModel.External;
import org.erlide.core.erlang.util.BackendUtils;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.core.erlang.util.ModelUtils;
import org.erlide.core.preferences.OldErlangProjectProperties;
import org.erlide.jinterface.util.ErlLogger;
import org.osgi.service.prefs.BackingStoreException;

import com.google.common.collect.Lists;

import erlang.ErlideOpen;

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
public class ErlProject extends Openable implements IErlProject {

    /**
     * Whether the underlying file system is case sensitive.
     */
    protected static final boolean IS_CASE_SENSITIVE = !new File("Temp").equals(new File("temp")); //$NON-NLS-1$ //$NON-NLS-2$

    /**
     * The platform project this <code>IErlProject</code> is based on
     */
    protected IProject fProject;

    /**
     * A array with all the non-Erlang resources contained by this
     * PackageFragment
     */
    private Collection<IResource> nonErlangResources;

    private String fCachedExternalModules = null;
    private String fCachedExternalIncludes = null;
    private Collection<IPath> fCachedSourceDirs = null;
    private Collection<IPath> fCachedIncludeDirs = null;

    public ErlProject(final IProject project, final ErlElement parent) {
        super(parent, project.getName());
        fProject = project;
        nonErlangResources = null;
    }

    /**
     * Adds a builder to the build spec for the given project.
     */
    protected void addToBuildSpec(final String builderID) throws CoreException {
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
    protected boolean buildStructure(final IProgressMonitor pm)
            throws ErlModelException {
        final IResource r = getResource();
        // check whether the Erlang project can be opened
        if (r == null || !r.isAccessible() || !(r instanceof IContainer)) {
            throw newNotPresentException();
        }
        try {
            final IContainer c = (IContainer) r;
            final IResource[] elems = c.members();
            final List<IErlElement> children = new ArrayList<IErlElement>(
                    elems.length + 1);
            ErlLogger.debug(">>adding externals");
            addExternals(children);
            ErlLogger.debug("childcount %d", children.size());
            ErlLogger.debug(">>adding otp");
            addOtpExternals(children);
            ErlLogger.debug("childcount %d", children.size());
            final IErlModelManager modelManager = ErlangCore.getModelManager();
            for (final IResource element : elems) {
                if (element instanceof IFolder) {
                    final IFolder folder = (IFolder) element;
                    final IErlFolder erlFolder = (IErlFolder) modelManager
                            .create(folder);
                    children.add(erlFolder);
                } else if (element instanceof IFile) {
                    final IFile file = (IFile) element;
                    if (ErlideUtil.isErlangFileContentFileName(file.getName())) {
                        final IErlModule m = (IErlModule) modelManager
                                .create(file);
                        children.add(m);
                    }
                }
            }

            setChildren(children);

        } catch (final CoreException e) {
            e.printStackTrace();
            setChildren(new ArrayList<IErlModule>());
            return false;
        }
        return true;
    }

    private void addOtpExternals(final List<IErlElement> children) {
        if (ModelUtils.isExternalFilesProject(getProject())) {
            return;
        }
        final Backend backend = BackendUtils.getBuildOrIdeBackend(getProject());
        final String name = backend.getInfo().getName();
        children.add(new ErlOtpExternalReferenceEntryList(this, name, backend));
    }

    private void addExternals(final List<IErlElement> children) {
        final IProject project = getProject();
        if (ModelUtils.isExternalFilesProject(project)) {
            return;
        }
        final String externalIncludes = getExternalIncludesString();
        final String externalModules = getExternalModulesString();
        final Collection<IPath> includeDirs = getIncludeDirs();
        final List<String> projectIncludes = Lists.newArrayList();
        for (final IPath path : includeDirs) {
            if (path.isAbsolute() && !project.getLocation().isPrefixOf(path)) {
                final RpcCallSite backend = BackendUtils
                        .getBuildOrIdeBackend(getProject());
                final Collection<String> headers = ErlideOpen.getHeadersInDir(
                        backend, path.toPortableString());
                for (final String header : headers) {
                    projectIncludes.add(path.append(header).toPortableString());
                }
            }
        }
        if (externalIncludes.length() != 0 || externalModules.length() != 0
                || !projectIncludes.isEmpty()) {
            children.add(new ErlExternalReferenceEntryList(this, "Externals",
                    "externals", externalIncludes, projectIncludes,
                    externalModules));
        }
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
    public boolean contains(final IResource resource) {
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

    // /**
    // * TODO: Record a new marker denoting a classpath problem
    // */
    // void createCodeProblemMarker(final IErlModelStatus status) {
    // /*
    // * final IMarker marker = null; int severity; String[] arguments = new
    // * String[0]; final boolean isCycleProblem = false,
    // * isClasspathFileFormatProblem = false; switch (status.getCode()) {
    // *
    // * case IErlModelStatusConstants.INCOMPATIBLE_ERTS_LEVEL: final String
    // * setting = getOption( ErlangCore.CORE_INCOMPATIBLE_ERTS_LEVEL, true);
    // * if (ErlangCore.ERROR.equals(setting)) { severity =
    // * IMarker.SEVERITY_ERROR; } else if
    // * (ErlangCore.WARNING.equals(setting)) { severity =
    // * IMarker.SEVERITY_WARNING; } else { return; // setting == IGNORE }
    // * break;
    // *
    // * default: final IPath path = status.getPath(); if (path != null) {
    // * arguments = new String[] { path.toString() }; } if
    // * (ErlangCore.ERROR.equals(getOption(
    // * ErlangCore.CORE_INCOMPLETE_CLASSPATH, true))) { severity =
    // * IMarker.SEVERITY_ERROR; } else { severity = IMarker.SEVERITY_WARNING; }
    // * break; }
    // */
    // }

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
    public boolean equals(final Object o) {

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
        return ErlideUtil.hasErlangNature(fProject);
    }

    /**
     * Remove all markers denoting classpath problems
     */
    protected void flushCodepathProblemMarkers(final boolean flushCycleMarkers,
            final boolean flushCodepathFormatMarkers) {
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
                ErlLogger.warn(e);
            }
        }
    }

    /**
     * @see IErlElement
     */
    public Kind getKind() {
        return Kind.PROJECT;
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
    private int getErlangCommandIndex(final ICommand[] buildSpec) {

        for (int i = 0; i < buildSpec.length; ++i) {
            if (ErlangPlugin.BUILDER_ID.equals(buildSpec[i].getBuilderName())) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Returns an array of non-Erlang resources contained in the receiver.
     */
    public Collection<IResource> getNonErlangResources()
            throws ErlModelException {
        return getNonErlangResources(this);
    }

    /**
     * @see IErlProject#getProject()
     */
    public IProject getProject() {
        return fProject;
    }

    /**
     * @throws CoreException
     * @see IErlProject#getRequiredProjectNames()
     */
    public Collection<String> getRequiredProjectNames() throws CoreException {
        final List<String> result = Lists.newArrayList();
        final IProject[] prjs = getProject().getReferencedProjects();
        for (final IProject p : prjs) {
            result.add(p.getName());
        }
        return result;
    }

    /**
     * @see IErlElement
     */
    @Override
    public IResource getResource() {
        return getCorrespondingResource();
    }

    @Override
    public IResource getCorrespondingResource() {
        return fProject;
    }

    // /**
    // * @see IErlElement
    // */
    // @Override
    // public IResource getUnderlyingResource() throws ErlModelException {
    // if (!exists()) {
    // throw newNotPresentException();
    // }
    // return fProject;
    // }

    @Override
    public int hashCode() {
        if (fProject == null) {
            return super.hashCode();
        }
        return fProject.hashCode();
    }

    /**
     * Removes the given builder from the build spec for the given project.
     */
    protected void removeFromBuildSpec(final String builderID)
            throws CoreException {

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
     * Update the Erlang command in the build spec (replace existing one if
     * present, add one first if none).
     */
    private void setErlangCommand(final IProjectDescription description,
            final ICommand newCommand) throws CoreException {
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
     * @see IErlProject
     */
    public void setOutputLocation(final IPath path,
            final IProgressMonitor monitor) throws ErlModelException {
        if (path == null) {
            throw new IllegalArgumentException(Util.bind("path.nullPath")); //$NON-NLS-1$
        }
        if (path.equals(getOutputLocation())) {
            return;
        }
        // this.setRawClasspath(SetClasspathOperation.ReuseClasspath, path,
        // monitor);
    }

    /**
     * Sets the underlying kernel project of this Erlang project, and fills in
     * its parent and name. Called by IProject.getNature().
     * 
     * @see IProjectNature#setProject(IProject)
     */
    public void setProject(final IProject project) {
        fProject = project;
        fParent = ErlangCore.getModel();
        fName = project.getName();
    }

    public Collection<IErlModule> getModules() throws ErlModelException {
        final List<IErlModule> modulesForProject = ErlModel.getErlModelCache()
                .getModulesForProject(this);
        if (modulesForProject != null) {
            return modulesForProject;
        }
        final List<IErlModule> result = new ArrayList<IErlModule>();
        if (ModelUtils.isExternalFilesProject(fProject)) {
            for (final IErlElement child : getChildren()) {
                if (child instanceof IErlModule
                        && ErlideUtil.hasErlExtension(child.getName())) {
                    result.add((IErlModule) child);
                }
            }
        } else {
            final IOldErlangProjectProperties props = getProperties();
            result.addAll(getModulesOrIncludes(fProject, getModel(),
                    props.getSourceDirs()));
        }
        ErlModel.getErlModelCache().putModulesForProject(this, result);
        return result;
    }

    private static List<IErlModule> getModulesOrIncludes(
            final IProject project, final IErlModel model,
            final Collection<IPath> dirs) throws ErlModelException {
        final List<IErlModule> result = Lists.newArrayList();
        for (final IPath dir : dirs) {
            final IFolder folder = project.getFolder(dir);
            final IErlElement element = model.findElement(folder, true);
            if (element instanceof IErlFolder) {
                final IErlFolder erlFolder = (IErlFolder) element;
                erlFolder.open(null);
                for (final IErlElement e : erlFolder
                        .getChildrenOfKind(Kind.MODULE)) {
                    if (e instanceof IErlModule) {
                        final IErlModule m = (IErlModule) e;
                        result.add(m);
                    }
                }
            }
        }
        return result;
    }

    public Collection<IErlModule> getModulesAndHeaders()
            throws ErlModelException {
        final List<IErlModule> result = new ArrayList<IErlModule>();
        final ErlModelCache erlModelCache = ErlModel.getErlModelCache();
        final List<IErlModule> modulesForProject = erlModelCache
                .getModulesForProject(this);
        final List<IErlModule> includesForProject = erlModelCache
                .getIncludesForProject(this);
        if (modulesForProject != null && includesForProject != null) {
            result.addAll(modulesForProject);
            result.addAll(includesForProject);
        } else {
            if (ModelUtils.isExternalFilesProject(fProject)) {
                for (final IErlElement child : getChildren()) {
                    if (child instanceof IErlModule
                            && ErlideUtil.hasModuleExtension(child.getName())) {
                        result.add((IErlModule) child);
                    }
                }
            } else {
                final List<IErlModule> cached = erlModelCache
                        .getModulesForProject(this);
                final ErlModel model = getModel();
                if (cached != null) {
                    result.addAll(cached);
                } else {
                    final List<IErlModule> modules = getModulesOrIncludes(
                            fProject, model, getSourceDirs());
                    result.addAll(modules);
                }
                final Collection<IErlModule> includes = getHeaders();
                result.addAll(includes);
            }
        }
        return result;
    }

    public Collection<IErlModule> getHeaders() throws ErlModelException {
        final ErlModelCache erlModelCache = ErlModel.getErlModelCache();
        final List<IErlModule> cached = erlModelCache
                .getIncludesForProject(this);
        if (cached != null) {
            return cached;
        }
        final List<IErlModule> includes = getModulesOrIncludes(fProject,
                getModel(), getIncludeDirs());
        erlModelCache.putIncludesForProject(this, includes);
        return includes;
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
    public static IPath canonicalizedPath(final IPath externalPath) {

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
            canonicalPath = new Path(
                    new File(externalPath.toOSString()).getCanonicalPath());
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
    private Collection<IResource> getNonErlangResources(
            final IErlProject project) {

        if (nonErlangResources == null) {
            nonErlangResources = Lists.newArrayList();
        }
        return Collections.unmodifiableCollection(nonErlangResources);
    }

    public boolean isVisibleInOutline() {
        return false;
    }

    @Override
    protected void closing(final Object info) throws ErlModelException {
        // TODO Auto-generated method stub

    }

    public boolean isOnSourcePath() {
        return true; // FIXME eller? ska man kolla nature? fast det ar val
        // redan klart... kanske den inte ska arva fran
        // IErlFolder? jaja....
    }

    public boolean isOnIncludePath() {
        return false; // FIXME
    }

    IErlModule getModuleAux(final String name, final boolean ignoreCase)
            throws ErlModelException {
        final boolean hasExtension = ErlideUtil.hasExtension(name);
        final Collection<IErlModule> modules = getModules();
        for (final IErlModule module : modules) {
            final String moduleName = hasExtension ? module.getName() : module
                    .getModuleName();
            if (ignoreCase ? moduleName.equalsIgnoreCase(name) : moduleName
                    .equals(name)) {
                return module;
            }
        }
        return null;
    }

    public IErlModule getModule(final String name) throws ErlModelException {
        return getModuleAux(name, false);
    }

    public IErlModule getModuleIgnoreCase(final String name)
            throws ErlModelException {
        return getModuleAux(name, true);
    }

    public boolean isSourcePathParent() {
        return true;
    }

    private IOldErlangProjectProperties getProperties() {
        return new OldErlangProjectProperties(fProject);
    }

    public Collection<IErlModule> getExternalModules() throws ErlModelException {
        final List<IErlModule> result = Lists.newArrayList();
        accept(new IErlElementVisitor() {

            public boolean visit(final IErlElement element)
                    throws ErlModelException {
                final boolean isExternalOrProject = element.getKind() == Kind.EXTERNAL
                        || element.getKind() == Kind.PROJECT;
                if (element instanceof IErlModule) {
                    final IErlModule module = (IErlModule) element;
                    result.add(module);
                    return false;
                } else if (isExternalOrProject) {
                    if (element instanceof IOpenable) {
                        final IOpenable openable = (IOpenable) element;
                        openable.open(null);
                    }
                }
                return isExternalOrProject;
            }
        }, 0, Kind.MODULE);
        return result;
    }

    @Override
    public void resourceChanged(final IResourceDelta delta) {
        if (delta != null
                && (delta.getFlags() & IResourceDelta.DESCRIPTION) != 0) {
            // TODO when we have cache in ErlModuleMap for referenced projects,
            // we should purge it here
            int i = 0;
            ++i;
        }
        if ((delta.getFlags() & ~IResourceDelta.MARKERS) != 0) {
            super.resourceChanged(delta);
        }
    }

    private String getExternal(final External external) {
        final IPreferencesService service = Platform.getPreferencesService();
        final String key = external == External.EXTERNAL_INCLUDES ? "default_external_includes"
                : "default_external_modules";
        String result = getExternal(external, service, key, "org.erlide.ui");
        if ("".equals(result)) {
            result = getExternal(external, service, key, ErlangPlugin.PLUGIN_ID);
        }
        return result;
    }

    private String getExternal(final External external,
            final IPreferencesService service, final String key,
            final String pluginId) {
        final String s = service.getString(pluginId, key, "", null);
        if (s.length() > 0) {
            ErlLogger.debug("%s: '%s'", key, s);
        }
        final String global = s;
        final IOldErlangProjectProperties prefs = getProperties();
        final String projprefs = external == External.EXTERNAL_INCLUDES ? prefs
                .getExternalIncludesFile() : prefs.getExternalModulesFile();
        return PreferencesUtils.packArray(new String[] { projprefs, global });
    }

    public String getExternalModulesString() {
        if (fCachedExternalModules == null) {
            fCachedExternalModules = getExternal(External.EXTERNAL_MODULES);
        }
        return fCachedExternalModules;
    }

    public String getExternalIncludesString() {
        if (fCachedExternalIncludes == null) {
            fCachedExternalIncludes = getExternal(External.EXTERNAL_INCLUDES);
        }
        return fCachedExternalIncludes;
    }

    public void setIncludeDirs(final Collection<IPath> includeDirs)
            throws BackingStoreException {
        fCachedIncludeDirs = null;
        final IOldErlangProjectProperties properties = getProperties();
        properties.setIncludeDirs(includeDirs);
        properties.store();
    }

    public void setSourceDirs(final Collection<IPath> sourceDirs)
            throws BackingStoreException {
        fCachedSourceDirs = null;
        final IOldErlangProjectProperties properties = getProperties();
        properties.setSourceDirs(sourceDirs);
        properties.store();
    }

    public void setExternalModulesFile(final String absolutePath)
            throws BackingStoreException {
        fCachedExternalModules = null;
        final IOldErlangProjectProperties properties = getProperties();
        properties.setExternalModulesFile(absolutePath);
        properties.store();
    }

    public void setExternalIncludesFile(final String absolutePath)
            throws BackingStoreException {
        fCachedExternalIncludes = null;
        final IOldErlangProjectProperties properties = getProperties();
        properties.setExternalIncludesFile(absolutePath);
        properties.store();
    }

    public Collection<IPath> getSourceDirs() {
        if (fCachedSourceDirs == null) {
            final IOldErlangProjectProperties properties = getProperties();
            final Collection<IPath> sourceDirs = properties.getSourceDirs();
            fCachedSourceDirs = resolvePaths(sourceDirs);
        }
        return fCachedSourceDirs;
    }

    public Collection<IPath> getIncludeDirs() {
        if (fCachedIncludeDirs == null) {
            final IOldErlangProjectProperties properties = getProperties();
            final Collection<IPath> includeDirs = properties.getIncludeDirs();
            fCachedIncludeDirs = resolvePaths(includeDirs);
        }
        return fCachedIncludeDirs;
    }

    private Collection<IPath> resolvePaths(final Collection<IPath> includeDirs) {
        final IPathVariableManager pathVariableManager = ResourcesPlugin
                .getWorkspace().getPathVariableManager();
        final List<IPath> cachedIncludeDirs = Lists
                .newArrayListWithCapacity(includeDirs.size());
        for (final IPath includeDir : includeDirs) {
            final IPath resolvedPath = pathVariableManager
                    .resolvePath(includeDir);
            cachedIncludeDirs.add(resolvedPath);
        }
        return Collections.unmodifiableCollection(cachedIncludeDirs);
    }

    public IPath getOutputLocation() {
        final IOldErlangProjectProperties properties = getProperties();
        return properties.getOutputDir();
    }

    public RuntimeInfo getRuntimeInfo() {
        final IOldErlangProjectProperties properties = getProperties();
        return properties.getRuntimeInfo();
    }

    public RuntimeVersion getRuntimeVersion() {
        final IOldErlangProjectProperties properties = getProperties();
        return properties.getRuntimeVersion();
    }

    public boolean hasSourceDir(final IPath fullPath) {
        final IPath f = fullPath.removeFirstSegments(1);
        for (final IPath s : getSourceDirs()) {
            if (s.equals(f)) {
                return true;
            }
            if (fullPath.segmentCount() == 1 && s.toString().equals(".")) {
                return true;
            }
        }
        return false;
    }

    public void setAllProperties(final IOldErlangProjectProperties bprefs)
            throws BackingStoreException {
        clearPropertyCaches();
        final IOldErlangProjectProperties properties = getProperties();
        properties.copyFrom(bprefs);
        properties.store();
    }

    private void clearPropertyCaches() {
        fCachedExternalIncludes = null;
        fCachedExternalModules = null;
        fCachedIncludeDirs = null;
        fCachedSourceDirs = null;
    }

    @Override
    public void clearCaches() {
        ErlModel.getErlModelCache().removeForProject(this);
    }

    public IErlModule findExternalModuleFromPath(final String path)
            throws CoreException {
        final Collection<IErlModule> modules = getExternalModules();
        for (final IErlModule module : modules) {
            final String filePath = module.getFilePath();
            if (filePath != null && path != null
                    && StringUtils.equalFilePaths(path, filePath)) {
                return module;
            }
        }
        return null;
    }

    public Collection<IErlProject> getProjectReferences() throws CoreException {
        final List<IErlProject> result = Lists.newArrayList();
        for (final IProject project : getProject().getReferencedProjects()) {
            final IErlProject p = getModel().getErlangProject(project);
            if (p != null) {
                result.add(p);
            }
        }
        return result;
    }

    public Collection<IErlModule> getExternalHeaders() throws ErlModelException {
        final List<IErlModule> result = Lists.newArrayList();
        accept(new IErlElementVisitor() {

            public boolean visit(final IErlElement element)
                    throws ErlModelException {
                final boolean isExternalOrProject = element.getKind() == Kind.EXTERNAL
                        || element.getKind() == Kind.PROJECT;
                if (element instanceof IErlModule) {
                    final IErlModule module = (IErlModule) element;
                    result.add(module);
                    return false;
                } else if (isExternalOrProject) {
                    if (element instanceof IErlExternal) {
                        final IErlExternal external = (IErlExternal) element;
                        if (!external.hasHeaders()) {
                            return false;
                        }
                    }
                    if (element instanceof IOpenable) {
                        final IOpenable openable = (IOpenable) element;
                        openable.open(null);
                    }
                }
                return isExternalOrProject;
            }
        }, 0, Kind.MODULE);
        return result;
    }

    /**
     * Try to find include file
     * 
     * @param project
     *            the project with include dirs
     * @param fileNameOrPath
     *            the path to the include file
     * @param externalIncludes
     * @return the module for the include file
     * @throws BackendException
     * @throws CoreException
     */
    public IErlModule findIncludeFile(final String fileNameOrPath)
            throws CoreException, BackendException {
        final Collection<IErlModule> headers = getHeaders();
        IErlModule header = findIncludeFileAux(fileNameOrPath, headers);
        if (header != null) {
            return header;
        }
        final Collection<IErlModule> externalHeaders = getExternalHeaders();
        header = findIncludeFileAux(fileNameOrPath, externalHeaders);
        if (header != null) {
            return header;
        }
        final Collection<IErlModule> modules = getModules();
        return findIncludeFileAux(fileNameOrPath, modules);
    }

    private IErlModule findIncludeFileAux(final String fileNameOrPath,
            final Collection<IErlModule> headers) {
        for (final IErlModule header : headers) {
            if (header.getFilePath().equals(fileNameOrPath)
                    || header.getName().equals(fileNameOrPath)) {
                return header;
            }
        }
        return null;
    }

    void pathVarsChanged() {
        clearPropertyCaches();
        clearCaches();
    }

    static IErlModule getModuleFromCacheByNameOrPath(final ErlProject project,
            final String moduleName, final String modulePath) {
        final ErlModelCache erlModelCache = ErlModel.getErlModelCache();
        if (modulePath != null) {
            final IErlModule module = erlModelCache.getModuleByPath(modulePath);
            if (module != null
                    && (project == null || project.moduleInProject(module))) {
                return module;
            }
        }
        final Set<IErlModule> modules = erlModelCache
                .getModulesByName(moduleName);
        if (modules != null) {
            for (final IErlModule module : modules) {
                if (project == null || project.moduleInProject(module)) {
                    final IParent parent = module.getParent();
                    if (parent instanceof IErlElement) {
                        final IErlElement element = (IErlElement) parent;
                        if (element.getKind() != Kind.EXTERNAL) {
                            return module;
                        }
                    }
                }
            }
            for (final IErlModule module : modules) {
                if (project == null || project.moduleInProject(module)) {
                    return module;
                }
            }
        }
        return null;
    }

    private boolean moduleInProject(final IErlModule module) {
        final IErlProject project = module.getProject();
        if (project == null) {
            return false;
        }
        return equals(project);
    }

    public IErlModule getExternalModule(final String moduleName)
            throws CoreException {
        final IErlModule module = getModuleFromCacheByNameOrPath(this,
                moduleName, null);
        if (module != null) {
            return module;
        }
        final Collection<IErlModule> modules = getExternalModules();
        for (final IErlModule module1 : modules) {
            if (module1.getModuleName().equals(moduleName)
                    || module1.getName().equals(moduleName)) {
                return module1;
            }
        }
        return null;
    }

    public IErlModule findExternalModule(final String moduleName,
            final String modulePath, final boolean checkReferences,
            final boolean checkAllProjects) throws CoreException {
        IErlModule module = getModuleFromCacheByNameOrPath(this, moduleName,
                modulePath);
        if (module != null) {
            return module;
        }
        module = findExternalModuleFromPath(modulePath);
        if (module != null) {
            return module;
        }
        if (moduleName != null) {
            final String moduleFileName;
            if (!ErlideUtil.hasModuleExtension(moduleName)) {
                moduleFileName = moduleName + ".erl";
            } else {
                moduleFileName = moduleName;
            }
            module = getExternalModule(moduleFileName);
            if (module != null) {
                return module;
            }
        }
        final IErlModel model = ErlangCore.getModel();
        final List<IErlProject> projectsToSearch = Lists.newArrayList();
        if (checkReferences) {
            projectsToSearch.addAll(getProjectReferences());
        }
        if (checkAllProjects) {
            for (final IErlProject project : model.getErlangProjects()) {
                if (!projectsToSearch.contains(project)) {
                    projectsToSearch.add(project);
                }
            }
        }
        projectsToSearch.remove(this);
        for (final IErlProject project : projectsToSearch) {
            ErlLogger.debug("searching project %s", project.getName());
            module = project.findExternalModule(moduleName, modulePath, false,
                    false);
            if (module != null && module.isOnSourcePath()) {
                return module;
            }
        }
        return null;
    }

}
