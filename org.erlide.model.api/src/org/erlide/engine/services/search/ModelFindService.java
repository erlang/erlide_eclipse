package org.erlide.engine.services.search;

import java.util.Collection;

import org.eclipse.core.runtime.CoreException;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.erlang.IErlFunction;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.erlang.IErlPreprocessorDef;
import org.erlide.engine.model.erlang.IErlTypespec;
import org.erlide.engine.model.erlang.ISourceRange;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlElementLocator;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.util.ErlangFunction;

import com.ericsson.otp.erlang.OtpErlangRangeException;

public interface ModelFindService {
    ISourceRange findVariable(ISourceRange range, String variableName,
            String elementText) throws OtpErlangRangeException;

    IErlElement findInclude(IErlModule module, IErlProject project,
            OpenResult res, IErlElementLocator model) throws CoreException;

    IErlTypespec findTypespec(IErlModule module, String name)
            throws CoreException;

    IErlPreprocessorDef findPreprocessorDef(final IErlModule module,
            final String definedName, final ErlElementKind kind)
            throws CoreException;

    IErlPreprocessorDef findPreprocessorDef(
            final Collection<IErlProject> projects, final String moduleName,
            final String definedName, final ErlElementKind kind)
            throws CoreException;

    IErlElement findTypeDef(final IErlElementLocator model,
            final IErlModule module, String moduleName, final String typeName,
            final String modulePath, final IErlProject project,
            final IErlElementLocator.Scope scope) throws CoreException;

    IErlModule findModule(final IErlElementLocator model,
            final IErlProject project, final String moduleName,
            final String modulePath, final IErlElementLocator.Scope scope)
            throws ErlModelException;

    IErlFunction findFunction(final IErlElementLocator model,
            String moduleName, final ErlangFunction erlangFunction,
            final String modulePath, final IErlProject project,
            final IErlElementLocator.Scope scope, final IErlModule module)
            throws CoreException;

    String resolveMacroValue(final String definedName, final IErlModule module);

}
