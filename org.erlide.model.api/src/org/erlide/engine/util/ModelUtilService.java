package org.erlide.engine.util;

import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.erlide.engine.ErlModelException;
import org.erlide.engine.model.erlang.IErlFunction;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.erlang.IErlPreprocessorDef;
import org.erlide.engine.model.erlang.IErlTypespec;
import org.erlide.engine.model.erlang.ISourceRange;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlElementLocator;
import org.erlide.engine.model.root.IErlModel;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.search.OpenResult;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;

public interface ModelUtilService {

    ISourceRange findVariable(ISourceRange range, String variableName,
            String elementText) throws OtpErlangRangeException;

    IErlElement findInclude(IErlModule module, IErlProject project,
            OpenResult res, IErlElementLocator model) throws CoreException;

    IErlTypespec findTypespec(IErlModule module, String name)
            throws CoreException;

    Object getTarget(final IContainer container, final IPath path,
            final boolean checkResourceExistence);

    IErlProject getProject(final IErlElement element);

    IErlModule getModule(final IErlElement element);

    boolean isOtpModule(final IErlModule module);

    String[] getPredefinedMacroNames();

    List<IErlPreprocessorDef> getAllPreprocessorDefs(final IErlModule module,
            final ErlElementKind kind) throws CoreException;

    List<OtpErlangObject> getImportsAsList(final IErlModule mod);

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

    List<String> findUnitsWithPrefix(final String prefix,
            final IErlProject project, final boolean checkExternals,
            final boolean includes) throws ErlModelException;

    IErlModule getModuleFromExternalModulePath(final IErlModel model,
            final String modulePath) throws ErlModelException;

    String getExternalModulePath(final IErlElementLocator model,
            final IErlModule module);

    String getModuleInfo(IErlModule module);

}
