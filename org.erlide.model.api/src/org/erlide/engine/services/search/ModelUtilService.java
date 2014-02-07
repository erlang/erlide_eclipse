package org.erlide.engine.services.search;

import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.erlang.IErlPreprocessorDef;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlElementLocator;
import org.erlide.engine.model.root.IErlProject;

import com.ericsson.otp.erlang.OtpErlangObject;

public interface ModelUtilService {

    Object getTarget(final IContainer container, final IPath path,
            final boolean checkResourceExistence);

    IErlProject getProject(final IErlElement element);

    IErlModule getModule(final IErlElement element);

    boolean isOtpModule(final IErlModule module);

    String[] getPredefinedMacroNames();

    List<IErlPreprocessorDef> getAllPreprocessorDefs(final IErlModule module,
            final ErlElementKind kind) throws CoreException;

    List<OtpErlangObject> getImportsAsList(final IErlModule mod);

    List<String> findUnitsWithPrefix(final String prefix, final IErlProject project,
            final boolean checkExternals, final boolean includes)
            throws ErlModelException;

    IErlModule getModuleFromExternalModulePath(final IErlModel model,
            final String modulePath) throws ErlModelException;

    String getExternalModulePath(final IErlElementLocator model, final IErlModule module);

    String getModuleInfo(IErlModule module);

}
