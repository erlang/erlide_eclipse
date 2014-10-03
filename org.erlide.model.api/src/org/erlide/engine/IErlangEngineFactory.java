package org.erlide.engine;

import org.erlide.runtime.api.IOtpRpc;

public interface IErlangEngineFactory {

    IErlangEngine get(IOtpRpc backend);

}
