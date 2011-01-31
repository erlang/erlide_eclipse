package org.erlide.core.erlang.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceStatus;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
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
import org.erlide.core.erlang.IErlExternal;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlImport;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlModuleMap;
import org.erlide.core.erlang.IErlPreprocessorDef;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IErlTypespec;
import org.erlide.core.erlang.IOpenable;
import org.erlide.core.erlang.IParent;
import org.erlide.core.erlang.ISourceRange;
import org.erlide.core.erlang.SourceRange;
import org.erlide.core.util.StringUtils;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

import erlang.ErlideOpen;

public class ModelUtils {

    public static final String EXTERNAL_FILES_PROJECT_NAME = "External_Files";

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
     * @throws BackendException
     * @throws CoreException
     */
    public static String findIncludeFile(final IErlProject project,
            final String filePath, final String externalIncludes)
            throws CoreException, BackendException {
        if (project == null) {
            return filePath;
        }
        for (final IErlModule module : project.getModules()) {
            final List<IErlModule> allIncludedFiles = findAllIncludedFiles(
                    module, externalIncludes);
            for (final IErlModule includeFile : allIncludedFiles) {
                if (includeFile.getFilePath().equals(filePath)
                        || includeFile.getName().equals(filePath)) {
                    return includeFile.getFilePath();
                }
            }
        }
        return filePath;
    }

    public static IErlTypespec findTypespec(final IErlModule module,
            final String name, final String externalIncludes)
            throws CoreException, BackendException {
        IErlTypespec typespec = findTypespec(module, name);
        if (typespec != null) {
            return typespec;
        }
        final List<IErlModule> includedFiles = findAllIncludedFiles(module,
                externalIncludes);
        for (final IErlModule includedFile : includedFiles) {
            typespec = findTypespec(includedFile, name);
            if (typespec != null) {
                return typespec;
            }
        }
        return null;
    }

    private static IErlTypespec findTypespec(final IErlModule module,
            final String name) throws ErlModelException {
        for (final IErlElement element : module
                .getChildrenOfKind(Kind.TYPESPEC)) {
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
        for (final IErlElement element : module
                .getChildrenOfKind(Kind.FUNCTION)) {
            if (element instanceof IErlFunction) {
                final IErlFunction f = (IErlFunction) element;
                if (f.getFunction().equals(erlangFunction)) {
                    return f;
                }
            }
        }
        return null;
    }

    public static IErlModule getModule(final IFile file)
            throws ErlModelException {
        final IErlModel model = ErlangCore.getModel();
        model.open(null);
        final IErlModule module = model.findModule(file);
        if (module != null) {
            return module;
        }
        return (IErlModule) ErlangCore.getModelManager().create(file, model);
    }

    public static Collection<SourcePathProvider> getSourcePathProviders()
            throws CoreException {
        // TODO should be cached and listening to plugin changes?
        final List<SourcePathProvider> result = Lists.newArrayList();
        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        final IConfigurationElement[] elements = reg
                .getConfigurationElementsFor(ErlangPlugin.PLUGIN_ID,
                        "sourcePathProvider");
        for (final IConfigurationElement element : elements) {
            final SourcePathProvider provider = (SourcePathProvider) element
                    .createExecutableExtension("class");
            result.add(provider);
        }
        return result;
    }

    public static IErlModule findExternalModuleFromPath(final String path)
            throws CoreException {
        final Collection<IErlElement> children = ErlangCore.getModel()
                .getChildren();
        for (final IErlElement child : children) {
            if (child instanceof IErlProject) {
                final IErlProject project = (IErlProject) child;
                final List<IErlModule> result = findExternalModulesFromPath(
                        path, project);
                if (!result.isEmpty()) {
                    return result.get(0);
                }
            }
        }
        return null;
    }

    public static IErlModule openExternal(final IErlProject project,
            final String path) throws CoreException {
        if (project != null) {
            final List<IErlModule> result = findExternalModulesFromPath(path,
                    project);
            if (!result.isEmpty()) {
                return result.get(0);
            }
        }
        return createModuleInExternalFilesProject(path);
    }

    public static List<IErlModule> findExternalModulesFromPath(
            final String path, final IErlProject project) throws CoreException {
        final List<IErlModule> result = Lists.newArrayList();
        final Collection<IErlModule> modules = project.getExternalModules();
        for (final IErlModule module : modules) {
            final String filePath = module.getFilePath();
            if (filePath != null && StringUtils.equalFilePaths(path, filePath)) {
                result.add(module);
            }
        }
        return result;
    }

    public static List<IErlModule> findExternalModulesFromName(
            final String moduleName, final IErlProject project)
            throws CoreException {
        final List<IErlModule> result = Lists.newArrayList();
        final Collection<IErlModule> modules = project.getExternalModules();
        for (final IErlModule module : modules) {
            if (module.getModuleName().equals(moduleName)) {
                result.add(module);
            }
        }
        return result;
    }

    public static boolean isExternalFilesProject(final IProject project) {
        return project.getName().equals(EXTERNAL_FILES_PROJECT_NAME);
    }

    static public IErlModule createModuleInExternalFilesProject(
            final String path) throws CoreException {
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

    public static IProject getExternalFilesProject() throws CoreException {
        final String prjName = EXTERNAL_FILES_PROJECT_NAME;
        final IWorkspace ws = ResourcesPlugin.getWorkspace();
        final IProject project = ws.getRoot().getProject(prjName);
        if (!project.exists()) {
            project.create(null);
            project.open(null);
            final IProjectDescription description = project.getDescription();
            description.setNatureIds(new String[] { ErlangPlugin.NATURE_ID });
            project.setDescription(description, null);
        }
        if (!project.isOpen()) {
            project.open(null);
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
            element = (IErlElement) element.getParent();
        }
        return StringUtils.join(DELIMITER, Lists.reverse(result));
    }

    private static IErlExternal getElementWithExternalName(
            final IParent parent, final String segment)
            throws ErlModelException {
        for (final IErlElement i : parent.getChildrenOfKind(Kind.EXTERNAL)) {
            final IErlExternal external = (IErlExternal) i;
            if (external.getExternalName().equals(segment)) {
                return external;
            }
        }
        return null;
    }

    public static IErlModule getModuleFromExternalModulePath(
            final String modulePath) throws ErlModelException {
        final List<String> path = StringUtils.split(DELIMITER, modulePath);
        IParent parent = ErlangCore.getModel().getErlangProject(path.get(0));
        final int n = path.size() - 1;
        for (int i = 1;; i++) {
            if (parent == null) {
                break;
            }
            if (parent instanceof IOpenable) {
                final IOpenable openable = (IOpenable) parent;
                openable.open(null);
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

    public static List<String> getExternalModulesWithPrefix(final Backend b,
            final String prefix, final IErlProject erlProject)
            throws CoreException {
        final List<String> result = Lists.newArrayList();
        final Collection<IErlModule> modules = erlProject.getExternalModules();
        for (final IErlModule module : modules) {
            final String name = module.getModuleName();
            if (name.startsWith(prefix)) {
                result.add(name);
            }
        }
        return result;
    }

    public static List<IErlModule> findAllIncludedFiles(
            final IErlModule module, final String externalIncludes)
            throws CoreException, BackendException {
        final List<IErlModule> checked = Lists.newArrayList(module);
        return findAllIncludedFiles(module, checked, externalIncludes);
    }

    private static List<IErlModule> findAllIncludedFiles(
            final IErlModule module, final List<IErlModule> checked,
            final String externalIncludes) throws CoreException,
            BackendException {
        final List<IErlModule> includedFilesForModule = ErlangCore
                .getModuleMap().getIncludedFilesForModule(module);
        if (includedFilesForModule != null && !includedFilesForModule.isEmpty()) {
            return includedFilesForModule;
        }
        final List<IErlModule> result = Lists.newArrayList();
        final Collection<ErlangIncludeFile> includes = module
                .getIncludedFiles();
        final IResource resource = module.getResource();
        final IErlProject erlProject = module.getProject();
        final IProject project = erlProject == null ? null : erlProject
                .getProject();
        final Backend backend = BackendUtils.getBuildOrIdeBackend(project);
        for (final ErlangIncludeFile element : includes) {
            IResource re = null;
            if (resource != null) {
                re = ResourceUtil
                        .recursiveFindNamedModuleResourceWithReferences(
                                project, element.getFilenameLastPart(),
                                PluginUtils
                                        .getIncludePathFilterCreator(resource
                                                .getParent()));
            }
            IErlModule includeModule = null;
            if (re instanceof IFile) {
                includeModule = getModule((IFile) re);
            } else {
                includeModule = getExternalInclude(backend, erlProject,
                        externalIncludes, element);
            }
            if (includeModule != null && !checked.contains(includeModule)) {
                checked.add(includeModule);
                result.add(includeModule);
                result.addAll(findAllIncludedFiles(includeModule, checked,
                        externalIncludes));
            }
        }
        ErlangCore.getModuleMap().setIncludedFilesForModule(module, result);
        return result;
    }

    public static IErlModule getExternalInclude(final Backend backend,
            final IErlProject project, final String externalIncludes,
            final ErlangIncludeFile element) throws BackendException,
            CoreException {
        String s = element.getFilename();
        if (element.isSystemInclude()) {
            s = ErlideOpen.getIncludeLib(backend, s);
        }
        // else {
        // s = findIncludeFile(project, s, externalIncludes);
        // }
        final IErlModule module = openExternal(project, s);
        return module;
    }

    public static String resolveMacroValue(final String definedName,
            final IErlModule m) {
        if ("?MODULE".equals(definedName)) {
            return m.getModuleName();
        }
        final IErlPreprocessorDef def = m.findPreprocessorDef(
                ErlideUtil.withoutInterrogationMark(definedName),
                Kind.MACRO_DEF);
        if (def != null) {
            final String extra = def.getExtra();
            final int p = extra.indexOf(',');
            if (p != -1) {
                final String s = extra.substring(p + 1).trim();
                if (s.length() > 0) {
                    return s;
                }
            }
        }
        return definedName;
    }

    public static IErlElement findExternalFunction(String moduleName,
            final ErlangFunction erlangFunction, final String modulePath,
            final IErlProject project, final boolean checkAllProjects,
            final IErlModule module) throws CoreException {
        if (moduleName != null) {
            moduleName = resolveMacroValue(moduleName, module);
            final IErlModule module2 = findExternalModule(moduleName,
                    modulePath, project, checkAllProjects);
            if (module2 != null) {
                module2.open(null);
                final IErlFunction function = module2
                        .findFunction(erlangFunction);
                if (function != null) {
                    return function;
                }
                return module2;
            }
        }
        return null;
    }

    public static IErlElement findExternalType(final IErlModule module,
            String moduleName, final String typeName, final String modulePath,
            final IErlProject project, final boolean checkAllProjects)
            throws CoreException {
        moduleName = resolveMacroValue(moduleName, module);
        final IErlModule module2 = findExternalModule(moduleName, modulePath,
                project, checkAllProjects);
        if (module2 != null) {
            module2.open(null);
            return module2.findTypespec(typeName);
        }
        return null;
    }

    public static IErlModule findExternalModule(final String moduleName,
            final String modulePath, final IErlProject project,
            final boolean checkAllProjects) throws CoreException {
        IErlModule module = getModuleByName(moduleName, modulePath, project);
        if (module == null) {
            final String moduleFileName = moduleName + ".erl";
            IResource r = null;
            if (project != null) {
                r = ResourceUtil
                        .recursiveFindNamedModuleResourceWithReferences(
                                project.getProject(), moduleFileName,
                                PluginUtils.getSourcePathFilterCreator());

                if (r == null) {
                    if (checkAllProjects) {
                        final IWorkspaceRoot workspaceRoot = ResourcesPlugin
                                .getWorkspace().getRoot();
                        final IProject[] projects = workspaceRoot.getProjects();
                        for (final IProject p : projects) {
                            if (ErlideUtil.hasErlangNature(p)) {
                                ErlLogger.debug("searching project %s",
                                        p.getName());
                                r = ResourceUtil.recursiveFindNamedResource(p,
                                        moduleFileName,
                                        PluginUtils.getSourcePathFilter(p));
                                if (r != null) {
                                    ErlLogger.debug("found %s", r);
                                    break;
                                }
                            }
                        }
                    }
                    if (r == null) {
                        module = openExternal(project, modulePath);
                    }
                }
            }
            if (r instanceof IFile) {
                module = ErlangCore.getModel().findModule((IFile) r);
            }
        }
        return module;
    }

    public static IErlModule getModuleByName(final String moduleName,
            final String modulePath, final IErlProject project) {
        final IErlModuleMap modelMap = ErlangCore.getModuleMap();
        final Set<IErlModule> modules = modelMap.getModulesByName(moduleName);
        if (modules != null) {
            for (final IErlModule module : modules) {
                if (moduleInProject(module, project)) {
                    final IParent parent = module.getParent();
                    if (parent instanceof IErlElement) {
                        final IErlElement element = (IErlElement) parent;
                        if (element.getKind() != Kind.EXTERNAL) {
                            return module;
                        }
                    }
                }
            }
            if (modulePath != null) {
                final IErlModule module = modelMap.getModuleByPath(modulePath);
                if (module != null) {
                    return module;
                }
            }
            if (modulePath != null) {
                for (final IErlModule module : modules) {
                    final String filePath = module.getFilePath();
                    if (filePath != null && modulePath.equals(filePath)) {
                        return module;
                    }
                }
            }
            for (final IErlModule module : modules) {
                if (moduleInProject(module, project)) {
                    return module;
                }
            }
        }
        return null;
    }

    public static IErlModule getExternalModule(final String moduleName,
            final IErlProject project) throws CoreException {
        final IErlModule module = getModuleByName(moduleName, null, project);
        if (module != null) {
            return module;
        }
        final List<IErlModule> modules = findExternalModulesFromName(
                moduleName, project);
        if (!modules.isEmpty()) {
            return modules.get(0);
        }
        return null;
    }

    public static IErlPreprocessorDef findPreprocessorDef(
            final Collection<IErlProject> projects, final String moduleName,
            final String definedName, final IErlElement.Kind kind,
            final String externalIncludes) throws CoreException,
            BackendException {
        for (final IErlProject project : projects) {
            if (project != null) {
                final IErlModule module = project.getModule(moduleName);
                if (module != null) {
                    final IErlPreprocessorDef def = findPreprocessorDef(module,
                            definedName, kind, externalIncludes);
                    if (def != null) {
                        return def;
                    }
                }
            }
        }
        return null;
    }

    public static IErlPreprocessorDef findPreprocessorDef(
            final IErlModule module, final String definedName,
            final IErlElement.Kind kind, final String externalIncludes)
            throws CoreException, BackendException {
        String unquoted = ErlideUtil.unquote(definedName);
        final Set<String> names = new HashSet<String>(3);
        if (kind == Kind.RECORD_DEF) {
            while (names.add(unquoted)) {
                unquoted = resolveMacroValue(unquoted, module);
            }
        } else {
            names.add(unquoted);
        }
        names.add(definedName);
        final List<IErlModule> allIncludedFiles = findAllIncludedFiles(module,
                externalIncludes);
        allIncludedFiles.add(0, module);
        for (final IErlModule includedFile : allIncludedFiles) {
            for (final String name : names) {
                includedFile.open(null);
                final IErlPreprocessorDef preprocessorDef = includedFile
                        .findPreprocessorDef(name, kind);
                if (preprocessorDef != null) {
                    return preprocessorDef;
                }
            }
        }
        return null;
    }

    public static List<OtpErlangObject> getImportsAsList(final IErlModule mod) {
        if (mod == null) {
            return NO_IMPORTS;
        }
        final Collection<IErlImport> imports = mod.getImports();
        if (imports.isEmpty()) {
            return NO_IMPORTS;
        }
        final List<OtpErlangObject> result = new ArrayList<OtpErlangObject>(
                imports.size());
        for (final IErlImport i : imports) {
            final List<ErlangFunction> functions = i.getFunctions();
            final OtpErlangObject funsT[] = new OtpErlangObject[functions
                    .size()];
            int j = 0;
            for (final ErlangFunction f : functions) {
                funsT[j] = f.getNameArityTuple();
                j++;
            }
            final OtpErlangTuple modFunsT = new OtpErlangTuple(
                    new OtpErlangObject[] {
                            new OtpErlangAtom(i.getImportModule()),
                            new OtpErlangList(funsT) });
            result.add(modFunsT);
        }
        return result;
    }

    public static List<IErlPreprocessorDef> getPreprocessorDefs(
            final IErlModule module, final IErlElement.Kind kind,
            final String externalIncludes) throws CoreException,
            BackendException {
        final List<IErlPreprocessorDef> res = Lists.newArrayList();

        final List<IErlModule> modulesWithIncludes = findAllIncludedFiles(
                module, externalIncludes);
        modulesWithIncludes.add(module);
        for (final IErlModule m : modulesWithIncludes) {
            res.addAll(m.getPreprocessorDefs(kind));
        }
        return res;
    }

    public static final ArrayList<OtpErlangObject> NO_IMPORTS = new ArrayList<OtpErlangObject>(
            0);

    public static List<IErlModule> getModulesWithReferencedProjectsWithPrefix(
            final IErlProject project, final String prefix)
            throws CoreException {
        final IErlModel model = ErlangCore.getModel();
        final List<IErlModule> result = new ArrayList<IErlModule>();
        if (project == null) {
            return result;
        }
        project.open(null);
        addModulesWithPrefix(prefix, result, project.getModules());
        for (final IProject p : project.getProject().getReferencedProjects()) {
            final IErlProject ep = model.findProject(p);
            if (ep != null) {
                ep.open(null);
                addModulesWithPrefix(prefix, result, ep.getModules());
            }
        }
        return result;
    }

    private static void addModulesWithPrefix(final String prefix,
            final List<IErlModule> result, final Collection<IErlModule> modules) {
        for (final IErlModule module : modules) {
            if (module.getModuleName().startsWith(prefix)) {
                result.addAll(modules);
            }
        }
    }

    public static String[] getPredefinedMacroNames() {
        return new String[] { "MODULE", "LINE", "FILE" };
    }

    public static ISourceRange findVariable(final Backend backend,
            final ISourceRange range, final String variableName,
            final String elementText) throws OtpErlangRangeException {
        final OtpErlangTuple res2 = ErlideOpen.findFirstVar(backend,
                variableName, elementText);
        if (res2 != null) {
            final int relativePos = ((OtpErlangLong) res2.elementAt(0))
                    .intValue() - 1;
            final int length = ((OtpErlangLong) res2.elementAt(1)).intValue();
            final int start = relativePos + range.getOffset();
            return new SourceRange(start, length);
        }
        return range;
    }

    public static boolean moduleInProject(final IErlModule module,
            final IErlProject project) {
        final IErlProject project2 = module.getProject();
        if (project == null) {
            return true;
        }
        if (project2 == null) {
            return false;
        }
        return project.equals(project2);
    }
}
