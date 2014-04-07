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
package org.erlide.engine.internal.model.root;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.EnumSet;
import java.util.List;

import org.eclipse.core.resources.ICommand;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.internal.ModelPlugin;
import org.erlide.engine.internal.model.cache.ErlModelCache;
import org.erlide.engine.internal.model.erlang.ErlExternalReferenceEntryList;
import org.erlide.engine.internal.model.erlang.ErlOtpExternalReferenceEntryList;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.ErlModelStatus;
import org.erlide.engine.model.ErlModelStatusConstants;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.IOpenable;
import org.erlide.engine.model.IParent;
import org.erlide.engine.model.SourcePathUtils;
import org.erlide.engine.model.builder.BuilderProperties;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.erlang.SourceKind;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlElementLocator;
import org.erlide.engine.model.root.IErlElementVisitor;
import org.erlide.engine.model.root.IErlExternalRoot;
import org.erlide.engine.model.root.IErlFolder;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.model.root.IProjectConfigurator;
import org.erlide.engine.model.root.ProjectConfigType;
import org.erlide.engine.model.root.ProjectConfigurationChangeListener;
import org.erlide.engine.services.search.OpenService;
import org.erlide.engine.util.CommonUtils;
import org.erlide.engine.util.NatureUtil;
import org.erlide.runtime.api.RuntimeCore;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.util.ErlLogger;
import org.osgi.service.prefs.BackingStoreException;

import com.google.common.collect.Lists;

/**
 * Handle for an Erlang project.
 *
 * <p>
 * A Erlang Project internally maintains a devpath that corresponds to the
 * project's classpath. The classpath may include source folders from the
 * current project; archives in the current project, other projects, and the
 * local file system; and binary folders (output location) of other projects.
 * The Erlang Model presents source elements corresponding to output .beam files
 * in other projects, and thus uses the devpath rather than the classpath (which
 * is really a compilation path). The devpath mimics the classpath, except has
 * source folder entries in place of output locations in external projects.
 *
 * <p>
 * Each ErlProject has a NameLookup facility that locates elements on by name,
 * based on the devpath.
 *
 * @see IErlProject
 */
public class ErlProject extends Openable implements IErlProject,
        ProjectConfigurationChangeListener {

    private static final String CONFIG_TYPE_TAG = "configType";

    /**
     * Whether the underlying file system is case sensitive.
     */
    private static final boolean IS_CASE_SENSITIVE = !new File("Temp").equals(new File("temp")); //$NON-NLS-1$ //$NON-NLS-2$

    protected IProject fProject;
    private ProjectConfigType configType = ProjectConfigType.INTERNAL;
    private ErlangProjectProperties properties;
    private BuilderProperties builderProperties;

    private volatile boolean configuring = false;

    public ErlProject(final IProject project, final IParent parent) {
        super(parent, project.getName());
        fProject = project;
    }

    @Override
    public boolean buildStructure(final IProgressMonitor pm) throws ErlModelException {
        final IResource r = getCorrespondingResource();
        // check whether the Erlang project can be opened
        if (!(r instanceof IContainer) || !r.isAccessible()) {
            ErlLogger.warn("Project %s has no resources: res:%s acc:%s cont:%s",
                    getName(), r, r == null ? "?" : r.isAccessible(),
                    r instanceof IContainer);
            throw new ErlModelException(new ErlModelStatus(
                    ErlModelStatusConstants.ELEMENT_DOES_NOT_EXIST, this));
        }

        addConfigurationChangeListeners();

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
            final IErlModel model = ErlangEngine.getInstance().getModel();
            for (final IResource element : elems) {
                if (element instanceof IFolder) {
                    final IFolder folder = (IFolder) element;
                    final IErlFolder erlFolder = (IErlFolder) model.create(folder);
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
            ErlLogger.error(e);
            setChildren(new ArrayList<IErlModule>());
            return false;
        }
        return true;
    }

    private void addOtpExternals(final List<IErlElement> children) {
        final String name = "OTP " + getRuntimeVersion().toString();
        IErlElement external = getChildNamed(name);
        if (external == null) {
            external = new ErlOtpExternalReferenceEntryList(this, name);
        }
        children.add(external);
    }

    private void addExternals(final List<IErlElement> children) {
        final ErlangProjectProperties myProperties = getProperties();
        final String externalIncludes = myProperties.getExternalIncludes();
        final String externalModules = myProperties.getExternalModules();
        final Collection<IPath> includeDirs = myProperties.getIncludeDirs();
        final List<String> projectIncludes = Lists.newArrayList();
        for (final IPath path : includeDirs) {
            if (path.isAbsolute() && !fProject.getLocation().isPrefixOf(path)) {
                final Collection<String> includes = ErlangEngine.getInstance()
                        .getService(OpenService.class)
                        .getIncludesInDir(path.toPortableString());
                if (includes != null) {
                    for (final String include : includes) {
                        projectIncludes.add(path.append(include).toPortableString());
                    }
                }
            }
        }
        if (externalIncludes.length() != 0 || externalModules.length() != 0
                || !projectIncludes.isEmpty()) {
            final IErlExternalRoot external = new ErlExternalReferenceEntryList(this,
                    "Externals", externalIncludes, projectIncludes, externalModules);
            children.add(external);
        }
    }

    /**
     * Removes the Erlang nature from the project.
     */
    public void deconfigure() throws CoreException {
        // unregister Erlang builder
        removeFromBuildSpec(ModelPlugin.BUILDER_ID);
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
        return NatureUtil.hasErlangNature(fProject);
    }

    /**
     * @see IErlElement
     */
    @Override
    public ErlElementKind getKind() {
        return ErlElementKind.PROJECT;
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
    protected void removeFromBuildSpec(final String builderID) throws CoreException {

        final IProjectDescription description = fProject.getDescription();
        final ICommand[] commands = description.getBuildSpec();
        for (int i = 0; i < commands.length; ++i) {
            if (commands[i].getBuilderName().equals(builderID)) {
                final ICommand[] newCommands = new ICommand[commands.length - 1];
                System.arraycopy(commands, 0, newCommands, 0, i);
                System.arraycopy(commands, i + 1, newCommands, i, commands.length - i - 1);
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

    @Override
    public Collection<IErlModule> getModules() throws ErlModelException {
        final List<IErlModule> modulesForProject = ErlModelCache.getDefault()
                .getModulesForProject(this);
        if (modulesForProject != null) {
            return modulesForProject;
        }
        final List<IErlModule> result = new ArrayList<IErlModule>();
        final List<IPath> sourceDirs = Lists
                .newArrayList(getProperties().getSourceDirs());
        for (final IPath s : SourcePathUtils.getExtraSourcePathsForModel(fProject)) {
            sourceDirs.add(s);
        }
        result.addAll(getModulesOrIncludes(fProject, ErlangEngine.getInstance()
                .getModel(), sourceDirs, true));
        ErlModelCache.getDefault().putModulesForProject(this, result);
        return result;
    }

    private static List<IErlModule> getModulesOrIncludes(final IProject project,
            final IErlElementLocator model, final Collection<IPath> dirs,
            final boolean getModules) throws ErlModelException {
        final List<IErlModule> result = Lists.newArrayList();
        for (final IPath dir : dirs) {
            final IFolder folder = project.getFolder(dir);
            final IErlElement element = model.findElement(folder, true);
            if (element instanceof IErlFolder) {
                final IErlFolder erlFolder = (IErlFolder) element;
                erlFolder.open(null);
                for (final IErlElement e : erlFolder
                        .getChildrenOfKind(ErlElementKind.MODULE)) {
                    if (e instanceof IErlModule) {
                        final IErlModule m = (IErlModule) e;
                        final boolean isModule = SourceKind.nameToModuleKind(m.getName()) != SourceKind.HRL;
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
    public Collection<IErlModule> getModulesAndIncludes() throws ErlModelException {
        final List<IErlModule> result = new ArrayList<IErlModule>();
        final ErlModelCache erlModelCache = ErlModelCache.getDefault();
        final List<IErlModule> modulesForProject = erlModelCache
                .getModulesForProject(this);
        final List<IErlModule> includesForProject = erlModelCache
                .getIncludesForProject(this);
        if (modulesForProject != null && includesForProject != null) {
            result.addAll(modulesForProject);
            result.addAll(includesForProject);
        } else {
            final List<IErlModule> cached = erlModelCache.getModulesForProject(this);
            final IErlElementLocator model = ErlangEngine.getInstance().getModel();
            if (cached == null) {
                final List<IErlModule> modules = getModulesOrIncludes(fProject, model,
                        getProperties().getSourceDirs(), true);
                result.addAll(modules);
            } else {
                result.addAll(cached);
            }
            final Collection<IErlModule> includes = getIncludes();
            result.addAll(includes);
        }
        return result;
    }

    @Override
    public Collection<IErlModule> getIncludes() throws ErlModelException {
        final ErlModelCache erlModelCache = ErlModelCache.getDefault();
        final List<IErlModule> cached = erlModelCache.getIncludesForProject(this);
        if (cached != null) {
            return cached;
        }
        final List<IErlModule> includes = getModulesOrIncludes(fProject, ErlangEngine
                .getInstance().getModel(), getProperties().getIncludeDirs(), false);
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

    @Override
    public IErlModule getModule(final String name) {
        try {
            return ErlangEngine
                    .getInstance()
                    .getModel()
                    .findModuleFromProject(this, name, null, false,
                            IErlElementLocator.Scope.PROJECT_ONLY);
        } catch (final ErlModelException e) {
            // final boolean hasExtension = ListsUtils.hasExtension(name);
            return null;
        }
    }

    @Override
    public ErlangProjectProperties getProperties() {
        if (properties == null) {
            configurationChanged();
        }
        return properties;
    }

    public IEclipsePreferences getCorePropertiesNode() {
        return new ProjectScope(fProject).getNode("org.erlide.model");
    }

    @Override
    public Collection<IErlModule> getExternalModules() throws ErlModelException {
        final List<IErlModule> result = Lists.newArrayList();
        accept(new IErlElementVisitor() {

            @Override
            public boolean visit(final IErlElement element) throws ErlModelException {
                final boolean isExternalOrProject = element.getKind() == ErlElementKind.EXTERNAL_ROOT
                        || element.getKind() == ErlElementKind.EXTERNAL_APP
                        || element.getKind() == ErlElementKind.EXTERNAL_FOLDER
                        || element.getKind() == ErlElementKind.PROJECT;
                if (element instanceof IErlModule) {
                    final IErlModule module = (IErlModule) element;
                    result.add(module);
                    return false;
                } else if (isExternalOrProject && element instanceof IOpenable) {
                    final IOpenable openable = (IOpenable) element;
                    openable.open(null);
                }
                return isExternalOrProject;
            }
        }, EnumSet.noneOf(AcceptFlags.class), ErlElementKind.MODULE);
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
        }
        if ((delta.getFlags() & ~IResourceDelta.MARKERS) != 0) {
            super.resourceChanged(delta);
            // FIXME when should we call getModelCache().removeProject(this); ?
        }
    }

    public void setIncludeDirs(final Collection<IPath> includeDirs) {
        getModelCache().removeProject(this);
        properties.setIncludeDirs(includeDirs);
        storeProperties();
        setStructureKnown(false);
    }

    public void setSourceDirs(final Collection<IPath> sourceDirs) {
        getModelCache().removeProject(this);
        properties.setSourceDirs(sourceDirs);
        storeProperties();
        setStructureKnown(false);
    }

    public void setExternalModulesFile(final String absolutePath) {
        getModelCache().removeProject(this);
        properties.setExternalModulesFile(absolutePath);
        storeProperties();
        setStructureKnown(false);
    }

    public void setExternalIncludesFile(final String absolutePath) {
        getModelCache().removeProject(this);
        properties.setExternalIncludesFile(absolutePath);
        storeProperties();
        setStructureKnown(false);
    }

    RuntimeVersion cachedRuntimeVersion;
    RuntimeInfo cachedRuntimeInfo;

    private boolean storing;

    @Override
    public RuntimeInfo getRuntimeInfo() {
        final RuntimeVersion requiredRuntimeVersion = getProperties()
                .getRequiredRuntimeVersion();
        if (requiredRuntimeVersion != cachedRuntimeVersion) {
            cachedRuntimeVersion = requiredRuntimeVersion;
            cachedRuntimeInfo = RuntimeCore.getRuntimeInfoCatalog().getRuntime(
                    requiredRuntimeVersion, null);
        }
        return cachedRuntimeInfo;
    }

    @Override
    public RuntimeVersion getRuntimeVersion() {
        final RuntimeInfo runtimeInfo = getRuntimeInfo();
        if (runtimeInfo != null) {
            return runtimeInfo.getVersion();
        }
        // should not happen
        return null;
    }

    @Override
    public void setProperties(final ErlangProjectProperties newProperties) {
        getModelCache().removeProject(this);
        if (properties == null) {
            properties = newProperties;
        } else {
            properties.copyFrom(newProperties);
        }
        storeAllProperties();
    }

    @Override
    public void clearCaches() {
        getModelCache().removeProject(this);
    }

    @Override
    public Collection<IErlProject> getReferencedProjects() throws ErlModelException {
        final List<IErlProject> result = Lists.newArrayList();
        try {
            for (final IProject project : fProject.getReferencedProjects()) {
                final IErlProject p = ErlangEngine.getInstance().getModel()
                        .getErlangProject(project);
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
    public Collection<IErlModule> getExternalIncludes() throws ErlModelException {
        final List<IErlModule> result = Lists.newArrayList();
        accept(new IErlElementVisitor() {

            @Override
            public boolean visit(final IErlElement element) throws ErlModelException {
                final boolean isExternalOrProject = element.getKind() == ErlElementKind.EXTERNAL_ROOT
                        || element.getKind() == ErlElementKind.EXTERNAL_APP
                        || element.getKind() == ErlElementKind.EXTERNAL_FOLDER
                        || element.getKind() == ErlElementKind.PROJECT;
                if (element instanceof IErlModule) {
                    final IErlModule module = (IErlModule) element;
                    if (module.getSourceKind() == SourceKind.HRL) {
                        result.add(module);
                    }
                    return false;
                } else if (isExternalOrProject && element instanceof IOpenable) {
                    final IOpenable openable = (IOpenable) element;
                    openable.open(null);
                }
                return isExternalOrProject;
            }
        }, EnumSet.noneOf(AcceptFlags.class), ErlElementKind.MODULE);
        return result;
    }

    public void pathVarsChanged() {
        clearCaches();
    }

    public boolean moduleInProject(final IErlModule module) {
        final IErlProject project = ErlangEngine.getInstance().getModelUtilService()
                .getProject(module);
        if (project == null) {
            return false;
        }
        return equals(project);
    }

    @Override
    public void dispose() {
        removeConfigurationChangeListeners();
        clearCaches();
        try {
            accept(new IErlElementVisitor() {

                @Override
                public boolean visit(final IErlElement element) throws ErlModelException {
                    element.dispose();
                    return false;
                }
            }, EnumSet.of(AcceptFlags.CHILDREN_FIRST, AcceptFlags.LEAFS_ONLY),
                    ErlElementKind.MODULE);
        } catch (final ErlModelException e) {
        }
        super.dispose();
    }

    private void addConfigurationChangeListeners() {
        // TODO listen for changes to config files/prefs -- config specific??
    }

    private void removeConfigurationChangeListeners() {
        // TODO remove listeners above
    }

    @Override
    public IProject getWorkspaceProject() {
        return fProject;
    }

    @Override
    public void close() throws ErlModelException {
        clearCaches();
        super.close();
    }

    private void loadCoreProperties() {
        final IEclipsePreferences node = getCorePropertiesNode();
        final String name = node.get(CONFIG_TYPE_TAG, ProjectConfigType.INTERNAL.name());
        setConfigType(ProjectConfigType.valueOf(name));
    }

    private void storeCoreProperties() {
        final IEclipsePreferences node = getCorePropertiesNode();
        node.put(CONFIG_TYPE_TAG, getConfigType().name());
        try {
            node.flush();
        } catch (final Exception e) {
            ErlLogger.debug(e);
            // ignore?
        }
    }

    @Override
    public void setConfigType(final ProjectConfigType config) {
        if (configType != config) {
            configType = config;
            storeCoreProperties();
        }
    }

    @Override
    public ProjectConfigType getConfigType() {
        return configType;
    }

    private ErlangProjectProperties loadProperties() {
        final IProjectConfigurator builderConfig = getConfig();
        return builderConfig.getConfiguration();
    }

    private IProjectConfigurator getConfig() {
        return ErlangEngine.getInstance().getProjectConfiguratorFactory()
                .getConfig(getConfigType(), this);
    }

    private void storeProperties() {
        if (properties != null) {
            final IProjectConfigurator builderConfig = getConfig();
            builderConfig.setConfiguration(properties);
        }
    }

    @Override
    public void configurationChanged() {
        if (configuring || storing) {
            return;
        }
        try {
            configuring = true;
            loadAllProperties();
        } finally {
            configuring = false;
        }
    }

    private void loadAllProperties() {
        loadCoreProperties();
        loadBuilderProperties();
        properties = loadProperties();
    }

    @Override
    public void storeAllProperties() {
        if (storing) {
            return;
        }
        try {
            storing = true;
            storeCoreProperties();
            storeBuilderProperties();
            storeProperties();
        } finally {
            storing = false;
        }
    }

    private void loadBuilderProperties() {
        final IEclipsePreferences node = getCorePropertiesNode();
        final String data = node.get("builderData", "");
        builderProperties = new BuilderProperties();
        builderProperties.fromString(data);
        // TODO more
    }

    private void storeBuilderProperties() {
        final IEclipsePreferences node = getCorePropertiesNode();
        node.put("builderData", builderProperties.toString());
        try {
            node.flush();
        } catch (final BackingStoreException e) {
            // ignore?
        }
    }

    @Override
    public BuilderProperties getBuilderProperties() {
        if (builderProperties == null) {
            loadBuilderProperties();
        }
        return builderProperties;
    }

    @Override
    public void setBuilderProperties(final BuilderProperties props) {
        builderProperties = props;
    }

}
