package org.erlide.engine.services.codeassist;

import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.erlide.engine.model.root.IErlModule;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.ErlangService;
import org.erlide.runtime.rpc.IOtpRpc;

public interface CompletionService extends ErlangService {

    List<CompletionData> computeCompletions(IOtpRpc backend, IErlProject project,
            IErlModule module, String elementBefore, int offset, String before,
            boolean inString) throws CoreException;

}
