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
package org.erlide.core.internal.model.root;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;

import org.eclipse.core.resources.ICommand;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
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
import org.erlide.backend.BackendUtils;
import org.erlide.backend.IBackend;
import org.erlide.backend.runtimeinfo.RuntimeInfo;
import org.erlide.core.ErlangCore;
import org.erlide.core.internal.model.erlang.ErlExternalReferenceEntryList;
import org.erlide.core.internal.model.erlang.ErlOtpExternalReferenceEntryList;
import org.erlide.core.internal.model.root.ErlModel.External;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.erlang.ModuleKind;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlElementLocator;
import org.erlide.core.model.root.IErlElementVisitor;
import org.erlide.core.model.root.IErlExternal;
import org.erlide.core.model.root.IErlFolder;
import org.erlide.core.model.root.IErlModel;
import org.erlide.core.model.root.IErlModelMarker;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.model.root.IOldErlangProjectProperties;
import org.erlide.core.model.root.IOpenable;
import org.erlide.core.model.util.CoreUtil;
import org.erlide.core.model.util.ErlideUtil;
import org.erlide.core.services.search.ErlideOpen;
import org.erlide.jinterface.ErlLogger;
import org.erlide.utils.CommonUtils;
import org.erlide.utils.PreferencesUtils;
import org.osgi.service.prefs.BackingStoreException;

import com.ericsson.otp.erlang.RuntimeVersion;
import com.google.common.collect.Lists;

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
            // ErlLogger.debug(">>adding externals");
            addExternals(children);
            // ErlLogger.debug("childcount %d", children.size());
            // ErlLogger.debug(">>adding otp");
            addOtpExternals(children);
            // ErlLogger.debug("childcount %d", children.size());
            final IErlModel model = getModel();
            for (final IResource element : elems) {
                if (element instanceof IFolder) {
                    final IFolder folder = (IFolder) element;
                    final IErlFolder erlFolder = (IErlFolder) model
                            .create(folder);
                    children.add(erlFolder);
                } else if (element instanceof IFile) {
                    final IFile file = (IFile) element;
                    if (CommonUtils.isErlangFileContentFileName(file.getName())) {
                        final IErlModule m = (IErlModule) model.create(file);
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
        final IBackend backend = CoreUtil.getBuildOrIdeBackend(fProject);
        final String name = backend.getRuntimeInfo().getName();
        children.add(new ErlOtpExternalReferenceEntryList(this, name));
    }

    private void addExternals(final List<IErlElement> children) {
        final String externalIncludes = getExternalIncludesString();
        final String externalModules = getExternalModulesString();
        final Collection<IPath> includeDirs = getIncludeDirs();
        final List<String> projectIncludes = Lists.newArrayList();
        for (final IPath path : includeDirs) {
            if (path.isAbsolute() && !fProject.getLocation().isPrefixOf(path)) {
                final IBackend backend = CoreUtil
                        .getBuildOrIdeBackend(fProject);
                final Collection<String> includes = ErlideOpen
                        .getIncludesInDir(backend, path.toPortableString());
                if (includes != null) {
                    for (final String include : includes) {
                        projectIncludes.add(path.append(include)
                                .toPortableString());
                    }
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
        addToBuildSpec(ErlangCore.BUILDER_ID);
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
        removeFromBuildSpec(ErlangCore.BUILDER_ID);
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
        return fProject.equals(other.getWorkspaceProject());
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
            if (ModelConfig.verbose) {
                ErlLogger.warn(e);
            }
        }
    }

    /**
     * @see IErlElement
     */
    @Override
    public Kind getKind() {
        return Kind.PROJECT;
    }

    /**
     * Find the specific Erlang command amongst the given build spec and return
     * its index or -1 if not found.
     */
    private int getErlangCommandIndex(final ICommand[] buildSpec) {

        for (int i = 0; i < buildSpec.length; ++i) {
            if (ErlangCore.BUILDER_ID.equals(buildSpec[i].getBuilderName())) {
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

    @Override
    public Collection<IErlModule> getModules() throws ErlModelException {
        final List<IErlModule> modulesForProject = ErlModel.getErlModelCache()
                .getModulesForProject(this);
        if (modulesForProject != null) {
            return modulesForProject;
        }
        final List<IErlModule> result = new ArrayList<IErlModule>();
        final List<IPath> sourceDirs = Lists.newArrayList(getSourceDirs());
        for (final IPath s : BackendUtils.getExtraSourcePathsForModel(fProject)) {
            sourceDirs.add(s);
        }
        result.addAll(getModulesOrIncludes(fProject, getModel(), sourceDirs,
                true));
        ErlModel.getErlModelCache().putModulesForProject(this, result);
        return result;
    }

    private static List<IErlModule> getModulesOrIncludes(
            final IProject project, final IErlElementLocator model,
            final Collection<IPath> dirs, final boolean getModules)
            throws ErlModelException {
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
                        final boolean isModule = ModuleKind.nameToModuleKind(m
                                .getName()) != ModuleKind.HRL;
                        if (isModule == getModules) {
                            result.add(m);
                        }
                    }
                }
            }
        }
        return result;
    }

    @Override
    public Collection<IErlModule> getModulesAndIncludes()
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
            final List<IErlModule> cached = erlModelCache
                    .getModulesForProject(this);
            final IErlElementLocator model = getModel();
            if (cached != null) {
                result.addAll(cached);
            } else {
                final List<IErlModule> modules = getModulesOrIncludes(fProject,
                        model, getSourceDirs(), true);
                result.addAll(modules);
            }
            final Collection<IErlModule> includes = getIncludes();
            result.addAll(includes);
        }
        return result;
    }

    @Override
    public Collection<IErlModule> getIncludes() throws ErlModelException {
        final ErlModelCache erlModelCache = ErlModel.getErlModelCache();
        final List<IErlModule> cached = erlModelCache
                .getIncludesForProject(this);
        if (cached != null) {
            return cached;
        }
        final List<IErlModule> includes = getModulesOrIncludes(fProject,
                getModel(), getIncludeDirs(), false);
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

    @Override
    public IErlModule getModule(final String name) {
        try {
            return getModel().findModuleFromProject(this, name, null, false,
                    false, IErlElementLocator.Scope.PROJECT_ONLY);
        } catch (final ErlModelException e) {
            // final boolean hasExtension = ListsUtils.hasExtension(name);
            return null;
        }
    }

    private IOldErlangProjectProperties getProperties() {
        return new OldErlangProjectProperties(fProject);
    }

    @Override
    public Collection<IErlModule> getExternalModules() throws ErlModelException {
        final List<IErlModule> result = Lists.newArrayList();
        accept(new IErlElementVisitor() {

            @Override
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
        }, EnumSet.noneOf(AcceptFlags.class), Kind.MODULE);
        return result;
    }

    @Override
    public void resourceChanged(final IResourceDelta delta) {
        if (delta == null) {
            return;
        }
        if ((delta.getFlags() & IResourceDelta.DESCRIPTION) != 0) {
            // TODO when we have cache in ErlModuleMap for referenced projects,
            // we should purge it here
            int i = 0;
            ++i;
        }
        if ((delta.getFlags() & ~IResourceDelta.MARKERS) != 0) {
            super.resourceChanged(delta);
            // FIXME when should we call getModelCache().removeProject(this); ?
        }
    }

    private String getExternal(final External external) {
        final IPreferencesService service = Platform.getPreferencesService();
        final String key = external == External.EXTERNAL_INCLUDES ? "default_external_includes"
                : "default_external_modules";
        String result = getExternal(external, service, key, "org.erlide.ui");
        if ("".equals(result)) {
            result = getExternal(external, service, key, ErlangCore.PLUGIN_ID);
        }
        return result;
    }

    private String getExternal(final External external,
            final IPreferencesService service, final String key,
            final String pluginId) {
        final String s = service.getString(pluginId, key, "", null);
        if (s.length() > 0) {
            // ErlLogger.debug("%s: '%s'", key, s);
        }
        final String global = s;
        final IOldErlangProjectProperties prefs = getProperties();
        final String projprefs = external == External.EXTERNAL_INCLUDES ? prefs
                .getExternalIncludesFile() : prefs.getExternalModulesFile();
        return PreferencesUtils.packArray(new String[] { projprefs, global });
    }

    @Override
    public String getExternalModulesString() {
        final ErlModelCache modelCache = getModelCache();
        String externalModulesString = modelCache
                .getExternalModulesString(this);
        if (externalModulesString == null) {
            externalModulesString = getExternal(External.EXTERNAL_MODULES);
            modelCache.putExternalModulesString(this, externalModulesString);
        }
        return externalModulesString;
    }

    @Override
    public String getExternalIncludesString() {
        final ErlModelCache modelCache = getModelCache();
        String externalIncludesString = modelCache
                .getExternalIncludesString(this);
        if (externalIncludesString == null) {
            externalIncludesString = getExternal(External.EXTERNAL_INCLUDES);
            modelCache.putExternalIncludesString(this, externalIncludesString);
        }
        return externalIncludesString;
    }

    @Override
    public void setIncludeDirs(final Collection<IPath> includeDirs)
            throws BackingStoreException {
        getModelCache().removeProject(this);
        final IOldErlangProjectProperties properties = getProperties();
        properties.setIncludeDirs(includeDirs);
        properties.store();
        setStructureKnown(false);
    }

    @Override
    public void setSourceDirs(final Collection<IPath> sourceDirs)
            throws BackingStoreException {
        getModelCache().removeProject(this);
        final IOldErlangProjectProperties properties = getProperties();
        properties.setSourceDirs(sourceDirs);
        properties.store();
        setStructureKnown(false);
    }

    @Override
    public void setExternalModulesFile(final String absolutePath)
            throws BackingStoreException {
        getModelCache().removeProject(this);
        final IOldErlangProjectProperties properties = getProperties();
        properties.setExternalModulesFile(absolutePath);
        properties.store();
        setStructureKnown(false);
    }

    @Override
    public void setExternalIncludesFile(final String absolutePath)
            throws BackingStoreException {
        getModelCache().removeProject(this);
        final IOldErlangProjectProperties properties = getProperties();
        properties.setExternalIncludesFile(absolutePath);
        properties.store();
        setStructureKnown(false);
    }

    @Override
    public Collection<IPath> getSourceDirs() {
        final ErlModelCache modelCache = getModelCache();
        Collection<IPath> sourceDirs = modelCache.getSourceDirs(this);
        if (sourceDirs == null) {
            final IOldErlangProjectProperties properties = getProperties();
            sourceDirs = properties.getSourceDirs();
            sourceDirs = resolvePaths(sourceDirs);
            modelCache.putSourceDirs(this, sourceDirs);
        }
        return sourceDirs;
    }

    @Override
    public Collection<IPath> getIncludeDirs() {
        final ErlModelCache modelCache = getModelCache();
        Collection<IPath> includeDirs = modelCache.getIncludeDirs(this);
        if (includeDirs == null) {
            final IOldErlangProjectProperties properties = getProperties();
            includeDirs = properties.getIncludeDirs();
            includeDirs = resolvePaths(includeDirs);
            modelCache.putIncludeDirs(this, includeDirs);
        }
        return includeDirs;
    }

    private Collection<IPath> resolvePaths(final Collection<IPath> paths) {
        final IPathVariableManager pathVariableManager = ResourcesPlugin
                .getWorkspace().getPathVariableManager();
        final List<IPath> cachedIncludeDirs = Lists
                .newArrayListWithCapacity(paths.size());
        for (final IPath includeDir : paths) {
            @SuppressWarnings("deprecation")
            final IPath resolvedPath = pathVariableManager
                    .resolvePath(includeDir);
            cachedIncludeDirs.add(resolvedPath);
        }
        return Collections.unmodifiableCollection(cachedIncludeDirs);
    }

    @Override
    @Deprecated
    public IPath getOutputLocation() {
        return getProperties().getOutputDir();
    }

    public Collection<IPath> getOutputLocations() {
        return getProperties().getOutputDirs();
    }

    @Override
    public RuntimeInfo getRuntimeInfo() {
        return getProperties().getRuntimeInfo();
    }

    @Override
    public RuntimeVersion getRuntimeVersion() {
        return getProperties().getRuntimeVersion();
    }

    final IPath DOT_PATH = new Path(".");

    @Override
    public boolean hasSourceDir(final IPath path) {
        if (path.equals(DOT_PATH)) {
            return true;
        }
        final IPath f = path.removeFirstSegments(1);
        for (final IPath s : getSourceDirs()) {
            if (s.equals(f)) {
                return true;
            }
            // if (fullPath.segmentCount() == 1 && s.toString().equals(".")) {
            // return true;
            // }
        }
        return false;
    }

    @Override
    public void setAllProperties(final IOldErlangProjectProperties properties)
            throws BackingStoreException {
        getModelCache().removeProject(this);
        final IOldErlangProjectProperties projectProperties = getProperties();
        projectProperties.copyFrom(properties);
        projectProperties.store();
    }

    @Override
    public void clearCaches() {
        getModelCache().removeProject(this);
    }

    @Override
    public Collection<IErlProject> getReferencedProjects()
            throws ErlModelException {
        final List<IErlProject> result = Lists.newArrayList();
        try {
            for (final IProject project : fProject.getReferencedProjects()) {
                final IErlProject p = getModel().getErlangProject(project);
                if (p != null) {
                    result.add(p);
                }
            }
        } catch (final CoreException e) {
            throw new ErlModelException(e);
        }
        return result;
    }

    @Override
    public Collection<IErlModule> getExternalIncludes()
            throws ErlModelException {
        final List<IErlModule> result = Lists.newArrayList();
        accept(new IErlElementVisitor() {

            @Override
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
                        if (!external.hasIncludes()) {
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
        }, EnumSet.noneOf(AcceptFlags.class), Kind.MODULE);
        return result;
    }

    void pathVarsChanged() {
        clearCaches();
    }

    boolean moduleInProject(final IErlModule module) {
        final IErlProject project = module.getProject();
        if (project == null) {
            return false;
        }
        return equals(project);
    }

    @Override
    public void dispose() {
        clearCaches();
        try {
            accept(new IErlElementVisitor() {

                @Override
                public boolean visit(final IErlElement element)
                        throws ErlModelException {
                    element.dispose();
                    return false;
                }
            }, EnumSet.of(AcceptFlags.CHILDREN_FIRST, AcceptFlags.LEAFS_ONLY),
                    Kind.MODULE);
        } catch (final ErlModelException e) {
        }
        super.dispose();
    }

    @Override
    public IProject getWorkspaceProject() {
        return fProject;
    }
}
