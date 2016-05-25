package org.erlide.engine.services.codeassist;

import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.erlide.engine.services.ErlangService;
import org.erlide.runtime.rpc.IOtpRpc;

public interface CompletionService extends ErlangService {

    List<CompletionData> computeCompletions(IOtpRpc backend, int offset, String before0,
            boolean inString) throws CoreException;

}
