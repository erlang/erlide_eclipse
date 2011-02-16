package org.erlide.core.erlang.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.erlide.backend.util.StringUtils;
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

    public static IErlTypespec findTypespec(final IErlModule module,
            final String name) throws CoreException {
        IErlTypespec typespec = module.findTypespec(name);
        if (typespec != null) {
            return typespec;
        }
        final List<IErlModule> includedFiles = module.findAllIncludedFiles();
        for (final IErlModule includedFile : includedFiles) {
            typespec = includedFile.findTypespec(name);
            if (typespec != null) {
                return typespec;
            }
        }
        return null;
    }

    public static IErlModule findExternalModuleFromPath(final String path)
            throws CoreException {
        final Collection<IErlElement> children = ErlangCore.getModel()
                .getChildren();
        for (final IErlElement child : children) {
            if (child instanceof IErlProject) {
                final IErlProject project = (IErlProject) child;
                final IErlModule module = project
                        .findExternalModuleFromPath(path);
                if (module != null) {
                    return module;
                }
            }
        }
        return null;
    }

    public static boolean isExternalFilesProject(final IProject project) {
        return project.getName().equals(EXTERNAL_FILES_PROJECT_NAME);
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
            final String externalName = external.getExternalName();
            ErlLogger
                    .debug("externalName %s segment %s", externalName, segment);
            if (externalName.equals(segment)) {
                return external;
            }
        }
        return null;
    }

    public static IErlModule getModuleFromExternalModulePath(
            final String modulePath) throws ErlModelException {
        final List<String> path = StringUtils.split(DELIMITER, modulePath);
        final IErlElement childNamed = ErlangCore.getModel().getChildNamed(
                path.get(0));
        ErlLogger.debug(">>childNamed %s", (childNamed == null ? "<null>"
                : childNamed));
        if (childNamed instanceof IParent) {
            IParent parent = (IParent) childNamed;
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
                ErlLogger.debug(">>parent %s", (parent == null ? "<null>"
                        : parent));
            }
            if (parent != null) {
                final IErlElement child = parent.getChildNamed(path.get(n));
                if (child instanceof IErlModule) {
                    return (IErlModule) child;
                }
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

    public static String resolveMacroValue(final String definedName,
            final IErlModule m) {
        if ("?MODULE".equals(definedName)) {
            return m.getModuleName();
        }
        final IErlPreprocessorDef def = m.findPreprocessorDef(
                StringUtils.withoutInterrogationMark(definedName),
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

    public static IErlElement findFunction(String moduleName,
            final ErlangFunction erlangFunction, final String modulePath,
            final IErlProject project, final boolean checkAllProjects,
            final IErlModule module) throws CoreException {
        if (moduleName != null) {
            moduleName = resolveMacroValue(moduleName, module);
            IErlModule module2 = project.getModule(moduleName);
            if (module2 == null) {
                module2 = findExternalModule(moduleName, modulePath, project,
                        checkAllProjects);
            }
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
        if (module != null) {
            return module;
        }
        module = project.findExternalModuleFromPath(modulePath);
        if (module != null) {
            return module;
        }
        final String moduleFileName;
        if (!ErlideUtil.hasModuleExtension(moduleName)) {
            moduleFileName = moduleName + ".erl";
        } else {
            moduleFileName = moduleName;
        }
        module = getExternalModule(moduleFileName, project);
        if (module != null) {
            return module;
        }
        // final IResource r = null;
        final IErlModel model = ErlangCore.getModel();
        final List<IErlProject> projectsToSearch = Lists.newArrayList();
        if (project != null) {
            module = project.getModule(moduleName);
            if (module != null && module.isOnSourcePath()) {
                return module;
            }
            final Collection<IErlProject> references = project
                    .getProjectReferences();
            projectsToSearch.addAll(references);
        }
        if (checkAllProjects) {
            for (final IErlProject p : model.getErlangProjects()) {
                if (!projectsToSearch.contains(p)) {
                    projectsToSearch.add(p);
                }
            }
        }
        for (final IErlProject p : projectsToSearch) {
            ErlLogger.debug("searching project %s", p.getName());
            module = p.getModule(moduleName);
            if (module != null && module.isOnSourcePath()) {
                return module;
            }
        }
        return null;
    }

    private static IErlModule getModuleByName(final String moduleName,
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
            for (final IErlModule module : modules) {
                if (moduleInProject(module, project)) {
                    return module;
                }
            }
            if (modulePath != null) {
                final IErlModule module = modelMap.getModuleByPath(modulePath);
                if (module != null && moduleInProject(module, project)) {
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
        final Collection<IErlModule> modules = project.getExternalModules();
        for (final IErlModule module1 : modules) {
            if (module1.getModuleName().equals(moduleName)
                    || module1.getName().equals(moduleName)) {
                return module1;
            }
        }
        return null;
    }

    public static IErlPreprocessorDef findPreprocessorDef(
            final Collection<IErlProject> projects, final String moduleName,
            final String definedName, final IErlElement.Kind kind)
            throws CoreException, BackendException {
        for (final IErlProject project : projects) {
            if (project != null) {
                final IErlModule module = project.getModule(moduleName);
                if (module != null) {
                    final IErlPreprocessorDef def = findPreprocessorDef(module,
                            definedName, kind);
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
            final IErlElement.Kind kind) throws CoreException {
        String unquoted = StringUtils.unquote(definedName);
        final Set<String> names = new HashSet<String>(3);
        if (kind == Kind.RECORD_DEF) {
            while (names.add(unquoted)) {
                unquoted = resolveMacroValue(unquoted, module);
            }
        } else {
            names.add(unquoted);
        }
        names.add(definedName);
        final List<IErlModule> allIncludedFiles = module.findAllIncludedFiles();
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
            final Collection<ErlangFunction> functions = i.getFunctions();
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

    public static List<IErlPreprocessorDef> getAllPreprocessorDefs(
            final IErlModule module, final IErlElement.Kind kind)
            throws CoreException {
        final List<IErlPreprocessorDef> result = Lists.newArrayList();
        final List<IErlModule> modulesWithIncludes = module
                .findAllIncludedFiles();
        modulesWithIncludes.add(module);
        for (final IErlModule m : modulesWithIncludes) {
            result.addAll(m.getPreprocessorDefs(kind));
        }
        return result;
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

    private static boolean moduleInProject(final IErlModule module,
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
