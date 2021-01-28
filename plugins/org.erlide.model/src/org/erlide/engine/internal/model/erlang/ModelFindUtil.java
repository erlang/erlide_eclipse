package org.erlide.engine.internal.model.erlang;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.internal.model.SourceRange;
import org.erlide.engine.internal.model.cache.ErlModelCache;
import org.erlide.engine.model.ErlElementKind;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IErlElement;
import org.erlide.engine.model.IParent;
import org.erlide.engine.model.erlang.ErlangFunction;
import org.erlide.engine.model.erlang.ErlangIncludeFile;
import org.erlide.engine.model.erlang.IErlFunction;
import org.erlide.engine.model.erlang.IErlPreprocessorDef;
import org.erlide.engine.model.erlang.IErlTypespec;
import org.erlide.engine.model.erlang.ISourceRange;
import org.erlide.engine.model.erlang.SourceKind;
import org.erlide.engine.model.root.IErlElementLocator;
import org.erlide.engine.model.root.IErlExternal;
import org.erlide.engine.model.root.IErlModule;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.search.ModelFindService;
import org.erlide.engine.services.search.ModelUtilService;
import org.erlide.runtime.rpc.IOtpRpc;
import org.erlide.util.StringUtils;

import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

public class ModelFindUtil implements ModelFindService {

    private final ModelUtilService modelUtilService;

    public ModelFindUtil(final IOtpRpc backend) {
        modelUtilService = ErlangEngine.getInstance().getModelUtilService();
    }

    @Override
    public ISourceRange findVariable(final ISourceRange range, final String variableName,
            final String elementText) throws OtpErlangRangeException {
        final OtpErlangTuple res2 = ErlangEngine.getInstance().getOpenService()
                .findFirstVar(variableName, elementText);
        if (res2 != null) {
            final int relativePos = ((OtpErlangLong) res2.elementAt(0)).intValue() - 1;
            final int length = ((OtpErlangLong) res2.elementAt(1)).intValue();
            final int start = relativePos + range.getOffset();
            return new SourceRange(start, length);
        }
        return range;
    }

    @Override
    public IErlModule findInclude(final IErlElementLocator model,
            final IErlProject project, final IErlModule module, final String includeName,
            final String includePath) throws CoreException {
        if (module != null) {
            final IErlModule include = model.findIncludeFromModule(module, includeName,
                    includePath, IErlElementLocator.Scope.REFERENCED_PROJECTS);
            if (include != null) {
                return include;
            }
        } else if (project != null) {
            final IErlModule include = model.findIncludeFromProject(project, includeName,
                    includePath, IErlElementLocator.Scope.REFERENCED_PROJECTS);
            if (include != null) {
                return include;
            }
        }
        return null;
    }

    @Override
    public IErlTypespec findTypespec(final IErlModule module, final String name)
            throws CoreException {
        IErlTypespec typespec = module.findTypespec(name);
        if (typespec != null) {
            return typespec;
        }
        final Collection<IErlModule> includedFiles = findAllIncludedFiles(module);
        for (final IErlModule includedFile : includedFiles) {
            typespec = includedFile.findTypespec(name);
            if (typespec != null) {
                return typespec;
            }
        }
        return null;
    }

    @Override
    public IErlFunction findFunction(final IErlElementLocator model,
            final IErlProject project, final IErlModule module, final String moduleName0,
            final String modulePath, final ErlangFunction erlangFunction,
            final IErlElementLocator.Scope scope) throws CoreException {
        if (moduleName0 != null) {
            final String moduleName = resolveMacroValue(moduleName0, module);
            final IErlModule module2 = findModule(model, project, moduleName, modulePath,
                    scope);
            if (module2 != null) {
                module2.open(null);
                final IErlFunction function = module2.findFunction(erlangFunction);
                return function;
            }
        }
        return null;
    }

    @Override
    public IErlModule findModule(final IErlElementLocator model,
            final IErlProject project, final String moduleName, final String modulePath,
            final IErlElementLocator.Scope scope) throws ErlModelException {
        if (project != null) {
            return model.findModuleFromProject(project, moduleName, modulePath, scope);
        }
        if (scope == IErlElementLocator.Scope.ALL_PROJECTS) {
            return model.findModule(moduleName, modulePath);
        }
        return null;
    }

    @Override
    public IErlElement findTypeDef(final IErlElementLocator model,
            final IErlProject project, final IErlModule module, final String moduleName0,
            final String typeName, final String modulePath,
            final IErlElementLocator.Scope scope) throws CoreException {
        final String moduleName = resolveMacroValue(moduleName0, module);
        final IErlModule module2 = findModule(model, project, moduleName, modulePath,
                scope);
        if (module2 != null) {
            module2.open(null);
            return module2.findTypespec(typeName);
        }
        return null;
    }

    @Override
    public IErlPreprocessorDef findPreprocessorDef(final Collection<IErlProject> projects,
            final String moduleName, final String definedName, final ErlElementKind kind)
            throws CoreException {
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

    @Override
    public IErlPreprocessorDef findPreprocessorDef(final IErlModule module,
            final String definedName, final ErlElementKind kind) throws CoreException {
        String unquoted = StringUtils.unquote(definedName);
        final String quoted = StringUtils.quote(definedName);
        final Set<String> names = new HashSet<>(3);
        if (kind == ErlElementKind.RECORD_DEF) {
            while (names.add(unquoted)) {
                unquoted = resolveMacroValue(unquoted, module);
            }
        } else {
            names.add(unquoted);
        }
        names.add(quoted);
        names.add(definedName);
        final List<IErlModule> allIncludedFiles = Lists
                .newArrayList(findAllIncludedFiles(module));
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

    @Override
    public String resolveMacroValue(final String definedName, final IErlModule module) {
        if (module != null) {
            if ("?MODULE".equals(definedName)) {
                return module.getModuleName();
            }
            final IErlPreprocessorDef def = module.findPreprocessorDef(
                    ModelFindUtil.withoutInterrogationMark(definedName),
                    ErlElementKind.MACRO_DEF);
            if (def != null) {
                final String extra = def.getExtra();
                final int p = extra.indexOf(',');
                if (p != -1) {
                    final String s = extra.substring(p + 1).trim();
                    if (!s.isEmpty()) {
                        return s;
                    }
                }
            }
        }
        return definedName;
    }

    private static String withoutInterrogationMark(final String definedName) {
        if (definedName.startsWith("?")) {
            return definedName.substring(1);
        }
        return definedName;
    }

    @Override
    public Collection<IErlModule> findAllIncludedFiles(final IErlModule module)
            throws CoreException {
        final List<IErlModule> checked = Lists.newArrayList();
        return findAllIncludedFiles(module, checked);
    }

    public Collection<IErlModule> findAllIncludedFiles(final IErlModule module,
            final List<IErlModule> checked) throws CoreException {
        final Collection<IErlModule> result = Sets.newHashSet();

        if (checked.contains(module)) {
            return result;
        }
        checked.add(module);

        final List<IErlModule> includedFilesForModule = ErlModelCache.getDefault()
                .getIncludedFilesForModule(module);
        if (includedFilesForModule != null && !includedFilesForModule.isEmpty()) {
            return includedFilesForModule;
        }
        final Collection<ErlangIncludeFile> includedFiles = module.getIncludeFiles();
        final IErlProject project = modelUtilService.getProject(module);
        if (project == null) {
            return result;
        }
        final Collection<IErlModule> includes = project.getIncludes();
        includes.addAll(getLocalIncludes(module));
        Collection<IErlModule> externalIncludes = null;
        Collection<IErlModule> referencedIncludes = null;
        Collection<IErlModule> modules = null;
        for (final ErlangIncludeFile includeFile : includedFiles) {
            final String includeFileName = includeFile.getFilenameLastPart();
            if (findAllIncludedFilesAux(checked, result, includes, includeFileName)) {
                continue;
            }
            if (referencedIncludes == null) {
                referencedIncludes = Lists.newArrayList();
                final Collection<IErlProject> referencedProjects = project
                        .getReferencedProjects();
                for (final IErlProject referencedProject : referencedProjects) {
                    referencedIncludes.addAll(referencedProject.getIncludes());
                }
            }
            if (findAllIncludedFilesAux(checked, result, referencedIncludes,
                    includeFileName)) {
                continue;
            }
            if (externalIncludes == null) {
                externalIncludes = project.getExternalIncludes();
            }
            if (findAllIncludedFilesAux(checked, result, externalIncludes,
                    includeFileName)) {
                continue;
            }
            if (modules == null) {
                modules = project.getModules();
            }
            findAllIncludedFilesAux(checked, result, modules, includeFileName);
        }
        ErlModelCache.getDefault().putIncludedFilesForModule(module, result);
        return result;
    }

    private Collection<IErlModule> getLocalIncludes(final IErlModule module)
            throws ErlModelException {
        final List<IErlModule> result = Lists.newArrayList();
        final IParent parent = module.getParent();
        for (final IErlElement child : parent.getChildrenOfKind(ErlElementKind.MODULE)) {
            if (child instanceof IErlModule
                    && SourceKind.nameToModuleKind(child.getName()) == SourceKind.HRL) {
                result.add((IErlModule) child);
            }
        }
        return result;
    }

    private boolean findAllIncludedFilesAux(final List<IErlModule> checked,
            final Collection<IErlModule> result, final Collection<IErlModule> includes,
            final String includeFileName) throws CoreException {
        for (final IErlModule include : includes) {
            if (include.getName().equals(includeFileName)) {
                if (include.getParent() instanceof IErlExternal) {
                    result.add(ModelFindUtil.findExternalIncludeInOpenProjects(include));
                } else {
                    result.add(include);
                }
                result.addAll(findAllIncludedFiles(include, checked));
                return true;
            }
        }
        return false;
    }

    public static IErlModule findExternalIncludeInOpenProjects(
            final IErlModule externalInclude) throws CoreException {
        final String filePath = externalInclude.getFilePath();
        final Collection<IErlProject> projects = ErlangEngine.getInstance().getModel()
                .getErlangProjects();
        for (final IErlProject project : projects) {
            final Collection<IErlModule> includes = project.getIncludes();
            for (final IErlModule include : includes) {
                if (include.getFilePath().equals(filePath)) {
                    return include;
                }
            }
        }
        return externalInclude;
    }

}
