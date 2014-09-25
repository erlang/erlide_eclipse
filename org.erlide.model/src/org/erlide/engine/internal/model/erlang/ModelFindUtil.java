package org.erlide.engine.internal.model.erlang;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.erlang.ErlangFunction;
import org.erlide.engine.model.erlang.IErlFunction;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.erlang.IErlPreprocessorDef;
import org.erlide.engine.model.erlang.IErlTypespec;
import org.erlide.engine.model.erlang.ISourceRange;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlElementLocator;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.search.ModelFindService;
import org.erlide.engine.services.search.OpenService;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.util.StringUtils;

import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

public class ModelFindUtil implements ModelFindService {

    public ModelFindUtil(final IOtpRpc backend) {
    }

    @Override
    public ISourceRange findVariable(final ISourceRange range, final String variableName,
            final String elementText) throws OtpErlangRangeException {
        final OtpErlangTuple res2 = ErlangEngine.getInstance()
                .getService(OpenService.class).findFirstVar(variableName, elementText);
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
        final Collection<IErlModule> includedFiles = ErlangEngine.getInstance()
                .getModelSearcherService().findAllIncludedFiles(module);
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
                if (function != null) {
                    return function;
                }
                return null;
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
    public IErlPreprocessorDef findPreprocessorDef(
            final Collection<IErlProject> projects, final String moduleName,
            final String definedName, final ErlElementKind kind) throws CoreException {
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
        final Set<String> names = new HashSet<String>(3);
        if (kind == ErlElementKind.RECORD_DEF) {
            while (names.add(unquoted)) {
                unquoted = resolveMacroValue(unquoted, module);
            }
        } else {
            names.add(unquoted);
        }
        names.add(quoted);
        names.add(definedName);
        final List<IErlModule> allIncludedFiles = Lists.newArrayList(ErlangEngine
                .getInstance().getModelSearcherService().findAllIncludedFiles(module));
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
                    StringUtils.withoutInterrogationMark(definedName),
                    ErlElementKind.MACRO_DEF);
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
        }
        return definedName;
    }

}
