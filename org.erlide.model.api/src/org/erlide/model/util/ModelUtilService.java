package org.erlide.model.util;

import org.eclipse.core.runtime.CoreException;
import org.erlide.model.erlang.IErlModule;
import org.erlide.model.erlang.ISourceRange;
import org.erlide.model.root.IErlElement;
import org.erlide.model.root.IErlElementLocator;
import org.erlide.model.root.IErlProject;
import org.erlide.model.services.search.OpenResult;
import org.erlide.runtime.api.IRpcSite;

import com.ericsson.otp.erlang.OtpErlangRangeException;

public interface ModelUtilService {

    public abstract ISourceRange findVariable(IRpcSite backend,
            ISourceRange range, String variableName, String elementText)
            throws OtpErlangRangeException;

    public abstract IErlElement findInclude(IErlModule module,
            IErlProject project, OpenResult res, IErlElementLocator model)
            throws CoreException;

}
