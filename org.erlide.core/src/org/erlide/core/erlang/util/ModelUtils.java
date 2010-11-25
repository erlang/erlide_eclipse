package org.erlide.core.erlang.util;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceStatus;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.RegistryFactory;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlElement.Kind;
import org.erlide.core.erlang.IErlElementVisitor;
import org.erlide.core.erlang.IErlExternal;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlPreprocessorDef;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IErlTypespec;
import org.erlide.core.erlang.IOldErlangProjectProperties;
import org.erlide.core.erlang.IOpenable;
import org.erlide.core.erlang.IParent;
import org.erlide.core.erlang.internal.ErlModelManager;
import org.erlide.core.util.StringUtils;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.util.ErlLogger;

import com.google.common.collect.Lists;

import erlang.ErlideOpen;

public class ModelUtils {

    private static final String DELIMITER = "<>";

    /**
     * Try to find include file, by searching include paths in the project
     * (replacing with path variables if needed). If the file is not in the
     * include paths, the original path is returned
     * 
     * @param project
     *            the project with include dirs
     * @param filePath
     *            the path to the include file
     * @param externalIncludes
     * @return the path to the include file
     */
    public static String findIncludeFile(final IProject project,
            final String filePath, final String externalIncludes) {
        if (project == null) {
            return filePath;
        }
        final IPathVariableManager pvm = ResourcesPlugin.getWorkspace()
                .getPathVariableManager();
        final IOldErlangProjectProperties prefs = ErlangCore
                .getProjectProperties(project);
        for (final IPath includeDir : prefs.getIncludeDirs()) {
            IPath path = includeDir.append(filePath);
            path = PluginUtils.resolvePVMPath(pvm, path);
            final File f = new File(path.toOSString());
            if (f.exists()) {
                return path.toString();
            }
        }
        final String s = ErlideOpen.getExternalInclude(ErlangCore
                .getBackendManager().getIdeBackend(), filePath,
                externalIncludes, ErlangCore.getModel().getPathVars());
        if (s != null) {
            return s;
        }
        return filePath;
    }

    public static IErlPreprocessorDef findPreprocessorDef(final Backend b,
            final Collection<IProject> projects, final String moduleName,
            final String definedName, final IErlElement.Kind type,
            final String externalIncludes) {
        try {
            final List<IErlModule> modulesDone = new ArrayList<IErlModule>();
            final IErlModel model = ErlModelManager.getDefault()
                    .getErlangModel();
            for (final IProject project : projects) {
                final IErlProject p = model.findProject(project);
                if (p != null) {
                    final IErlModule m = p.getModule(moduleName);
                    if (m != null) {
                        final IErlPreprocessorDef def = findPreprocessorDef(b,
                                projects, m, definedName, type,
                                externalIncludes, modulesDone);
                        if (def != null) {
                            return def;
                        }
                    }
                }
            }
        } catch (final CoreException e) {
        }
        return null;
    }

    /**
     * @param project
     * @param m
     * @param definedName
     * @param type
     * @param externalIncludes
     * @param modulesDone
     * @return
     * @throws CoreException
     */
    private static IErlPreprocessorDef findPreprocessorDef(final Backend b,
            final Collection<IProject> projects, IErlModule m,
            final String definedName, final IErlElement.Kind type,
            final String externalIncludes, final List<IErlModule> modulesDone)
            throws CoreException {
        if (m == null) {
            return null;
        }
        modulesDone.add(m);
        m.open(null);
        final IErlPreprocessorDef pd = m.findPreprocessorDef(definedName, type);
        if (pd != null) {
            return pd;
        }
        final Collection<ErlangIncludeFile> includes = m.getIncludedFiles();
        for (final ErlangIncludeFile element : includes) {
            IResource re = null;
            IProject project = null;
            for (final IProject p : projects) {
                re = ResourceUtil.recursiveFindNamedResourceWithReferences(p,
                        element.getFilenameLastPart(),
                        org.erlide.core.erlang.util.PluginUtils
                                .getIncludePathFilter(p, m.getResource()
                                        .getParent()));
                if (re != null) {
                    project = p;
                    break;
                }
            }
            if (re == null) {
                try {
                    String s = element.getFilename();
                    if (element.isSystemInclude()) {
                        s = ErlideOpen.getIncludeLib(b, s);
                    } else {
                        s = findIncludeFile(project, s, externalIncludes);
                    }
                    re = ResourceUtil.recursiveFindNamedResourceWithReferences(
                            project, s, org.erlide.core.erlang.util.PluginUtils
                                    .getIncludePathFilter(project, m
                                            .getResource().getParent()));
                } catch (final Exception e) {
                    // ErlLogger.warn(e);
                }
            } else if (re instanceof IFile) {
                m = ErlModelManager.getDefault().getErlangModel()
                        .findModule((IFile) re);
                if (m != null && !modulesDone.contains(m)) {
                    final IErlPreprocessorDef pd2 = findPreprocessorDef(b,
                            projects, m, definedName, type, externalIncludes,
                            modulesDone);
                    if (pd2 != null) {
                        return pd2;
                    }
                }
            }
        }
        return null;
    }

    public static IErlTypespec findTypespec(final IErlModule module,
            final String name) throws ErlModelException {
        final Collection<IErlElement> children = module.getChildren();
        for (final IErlElement element : children) {
            if (element instanceof IErlTypespec) {
                final IErlTypespec t = (IErlTypespec) element;
                if (t.getName().equals(name)) {
                    return t;
                }
            }
        }
        return null;
    }

    public static IErlFunction findFunction(final IErlModule module,
            final ErlangFunction erlangFunction) throws ErlModelException {
        final Collection<IErlElement> children = module.getChildren();
        for (final IErlElement element : children) {
            if (element instanceof IErlFunction) {
                final IErlFunction f = (IErlFunction) element;
                if (f.getFunction().equals(erlangFunction)) {
                    return f;
                }
            }
        }
        return null;
    }

    public static IErlModule getModule(final String moduleName) {
        final IErlModel model = ErlangCore.getModel();
        try {
            model.open(null);
            return model.findModule(moduleName);
        } catch (final ErlModelException e) {
        }
        return null;
    }

    public static IErlModule getModule(final IFile file) {
        final IErlModel model = ErlangCore.getModel();
        try {
            model.open(null);
            return model.findModule(file);
        } catch (final ErlModelException e) {
        }
        return null;
    }

    public static Collection<SourcePathProvider> getSourcePathProviders() {
        // TODO should be cached and listening to plugin changes?
        final List<SourcePathProvider> result = Lists.newArrayList();
        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        final IConfigurationElement[] elements = reg
                .getConfigurationElementsFor(ErlangPlugin.PLUGIN_ID,
                        "sourcePathProvider");
        for (final IConfigurationElement element : elements) {
            try {
                final SourcePathProvider provider = (SourcePathProvider) element
                        .createExecutableExtension("class");
                result.add(provider);
            } catch (final CoreException e) {
                e.printStackTrace();
            }
        }
        return result;
    }

    public static List<String> getExternalModules(final Backend b,
            final String prefix, final IErlModel model,
            final String externalModules) {
        return ErlideOpen.getExternalModules(b, prefix, externalModules,
                model.getPathVars());
    }

    public static List<String> getExternalModules(final Backend b,
            final String prefix, final IErlProject erlProject) {
        final IErlModel model = ErlangCore.getModel();
        final String externalModules = model.getExternalModules(erlProject);
        return getExternalModules(b, prefix, model, externalModules);
    }

    public static IErlModule findExternalModuleFromPath(final String path) {
        try {
            final Collection<IErlElement> children = ErlangCore.getModel()
                    .getChildren();
            for (final IErlElement child : children) {
                if (child instanceof IErlProject) {
                    final IErlProject erlProject = (IErlProject) child;
                    erlProject.open(null);
                    final Collection<IErlElement> externals = erlProject
                            .getChildrenOfKind(Kind.EXTERNAL);
                    final List<IErlModule> result = findExternalModuleFromPath(
                            path, externals);
                    if (!result.isEmpty()) {
                        return result.get(0);
                    }
                }
            }
        } catch (final CoreException e) {
        }
        return null;
    }

    public static IErlModule openExternal(final IProject project,
            final String path) throws CoreException {
        final IErlModel model = ErlangCore.getModel();
        final IErlProject erlProject = model.findProject(project);
        if (erlProject != null) {
            final Collection<IErlElement> children = erlProject
                    .getChildrenOfKind(Kind.EXTERNAL);
            final List<IErlModule> result = findExternalModuleFromPath(path,
                    children);
            if (!result.isEmpty()) {
                return result.get(0);
            }
        }
        return openInExternalFilesProject(path);
    }

    private static List<IErlModule> findExternalModuleFromPath(
            final String path, final Collection<IErlElement> children)
            throws ErlModelException {
        final List<IErlModule> result = Lists.newArrayList();
        for (final IErlElement external : children) {
            external.accept(new IErlElementVisitor() {

                public boolean visit(final IErlElement element)
                        throws ErlModelException {
                    final boolean isExternal = element.getKind() == Kind.EXTERNAL;
                    if (element instanceof IErlModule) {
                        final IErlModule module = (IErlModule) element;
                        final String filePath = module.getFilePath();
                        if (filePath != null && path.equals(filePath)) {
                            result.add(module);
                            return true;
                        }
                    } else if (isExternal) {
                        final IOpenable openable = (IOpenable) element;
                        openable.open(null);
                    }
                    return isExternal;
                }
            }, 0, Kind.MODULE);
        }
        return result;
    }

    static public IErlModule openInExternalFilesProject(final String path)
            throws CoreException {
        if (path == null) {
            return null;
        }
        final IProject project = getExternalFilesProject();
        final IFile file = project.getFile(new Path(path).lastSegment());
        createExternalFile(file, path, project);
        final IErlProject erlProject = ErlangCore.getModel().getErlangProject(
                project.getName());
        erlProject.open(null);
        return ErlangCore.getModel().findModule(file);
    }

    public static IProject getExternalFilesProject() {
        final String prjName = "External_Files";
        final IWorkspace ws = ResourcesPlugin.getWorkspace();
        final IProject project = ws.getRoot().getProject(prjName);
        if (!project.exists()) {
            try {
                project.create(null);
                project.open(null);
                final IProjectDescription description = project
                        .getDescription();
                description
                        .setNatureIds(new String[] { ErlangPlugin.NATURE_ID });
                project.setDescription(description, null);
            } catch (final CoreException e) {
                e.printStackTrace();
            }
        }
        if (!project.isOpen()) {
            try {
                project.open(null);
            } catch (final CoreException e) {
                e.printStackTrace();
            }
        }
        return project;
    }

    private static void createExternalFile(final IFile file, final String path,
            final IProject project) throws CoreException, ErlModelException {
        final IPath location = new Path(path);
        final IStatus status = ResourcesPlugin.getWorkspace()
                .validateLinkLocation(file, location);
        if (status.getSeverity() != IStatus.OK
                && status.getSeverity() != IStatus.INFO) {
            if (status.getSeverity() != IStatus.WARNING
                    || status.getCode() != IResourceStatus.OVERLAPPING_LOCATION) {
                ErlLogger.warn("Can't open %s:: %s", path, status.toString());
                return;
            }
        }
        if (!file.isLinked()) {
            file.createLink(location, IResource.NONE, null);
        }
    }

    public static String getExternalModulePath(final IErlModule module) {
        final List<String> result = Lists.newArrayList();
        IErlElement element = module;
        final IErlModel model = ErlangCore.getModel();
        while (element != model) {
            if (element instanceof IErlExternal) {
                final IErlExternal external = (IErlExternal) element;
                result.add(external.getExternalName());
            } else {
                result.add(element.getName());
            }
            element = element.getParent();
        }
        return StringUtils.join(DELIMITER, Lists.reverse(result));
    }

    private static IErlExternal getElementWithExternalName(
            final IParent parent, final String segment) {
        try {
            for (final IErlElement i : parent.getChildrenOfKind(Kind.EXTERNAL)) {
                final IErlExternal external = (IErlExternal) i;
                if (external.getExternalName().equals(segment)) {
                    return external;
                }
            }
        } catch (final ErlModelException e) {
        }
        return null;
    }

    public static IErlModule getModuleFromExternalModulePath(
            final String modulePath) {
        final List<String> path = StringUtils.split(DELIMITER, modulePath);
        IParent parent = ErlangCore.getModel().getErlangProject(path.get(0));
        final int n = path.size() - 1;
        for (int i = 1;; i++) {
            if (parent == null) {
                break;
            }
            if (parent instanceof IOpenable) {
                final IOpenable openable = (IOpenable) parent;
                try {
                    openable.open(null);
                } catch (final ErlModelException e) {
                }
            }
            if (i == n) {
                break;
            }
            parent = getElementWithExternalName(parent, path.get(i));
        }
        if (parent != null) {
            final IErlElement child = parent.getChildNamed(path.get(n));
            if (child instanceof IErlModule) {
                return (IErlModule) child;
            }
        }
        return null;
    }
}
