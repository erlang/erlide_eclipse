package org.erlide.model.util;

import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.erlide.model.ErlModelException;
import org.erlide.model.erlang.IErlFunction;
import org.erlide.model.erlang.IErlModule;
import org.erlide.model.erlang.IErlPreprocessorDef;
import org.erlide.model.erlang.IErlTypespec;
import org.erlide.model.erlang.ISourceRange;
import org.erlide.model.root.ErlElementKind;
import org.erlide.model.root.IErlElement;
import org.erlide.model.root.IErlElementLocator;
import org.erlide.model.root.IErlModel;
import org.erlide.model.root.IErlProject;
import org.erlide.model.services.search.OpenResult;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;

public interface ModelUtilService {

    public abstract ISourceRange findVariable(ISourceRange range,
            String variableName, String elementText)
            throws OtpErlangRangeException;

    public abstract IErlElement findInclude(IErlModule module,
            IErlProject project, OpenResult res, IErlElementLocator model)
            throws CoreException;

    IErlTypespec findTypespec(IErlModule module, String name)
            throws CoreException;

    public abstract Object getTarget(final IContainer container,
            final IPath path, final boolean checkResourceExistence);

    public abstract IErlProject getProject(final IErlElement element);

    public abstract IErlModule getModule(final IErlElement element);

    public abstract boolean isOtpModule(final IErlModule module);

    public abstract String[] getPredefinedMacroNames();

    public abstract List<IErlPreprocessorDef> getAllPreprocessorDefs(
            final IErlModule module, final ErlElementKind kind)
            throws CoreException;

    public abstract List<OtpErlangObject> getImportsAsList(final IErlModule mod);

    public abstract IErlPreprocessorDef findPreprocessorDef(
            final IErlModule module, final String definedName,
            final ErlElementKind kind) throws CoreException;

    public abstract IErlPreprocessorDef findPreprocessorDef(
            final Collection<IErlProject> projects, final String moduleName,
            final String definedName, final ErlElementKind kind)
            throws CoreException;

    public abstract IErlElement findTypeDef(final IErlElementLocator model,
            final IErlModule module, String moduleName, final String typeName,
            final String modulePath, final IErlProject project,
            final IErlElementLocator.Scope scope) throws CoreException;

    public abstract IErlModule findModule(final IErlElementLocator model,
            final IErlProject project, final String moduleName,
            final String modulePath, final IErlElementLocator.Scope scope)
            throws ErlModelException;

    public abstract IErlFunction findFunction(final IErlElementLocator model,
            String moduleName, final ErlangFunction erlangFunction,
            final String modulePath, final IErlProject project,
            final IErlElementLocator.Scope scope, final IErlModule module)
            throws CoreException;

    public abstract String resolveMacroValue(final String definedName,
            final IErlModule module);

    public abstract List<String> findUnitsWithPrefix(final String prefix,
            final IErlProject project, final boolean checkExternals,
            final boolean includes) throws ErlModelException;

    public abstract IErlModule getModuleFromExternalModulePath(
            final IErlModel model, final String modulePath)
            throws ErlModelException;

    public abstract String getExternalModulePath(
            final IErlElementLocator model, final IErlModule module);

}
