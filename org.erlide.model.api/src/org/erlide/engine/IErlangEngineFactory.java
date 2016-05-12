package org.erlide.engine;

import org.erlide.runtime.rpc.IOtpRpc;

public interface IErlangEngineFactory {

    IErlangEngine get(IOtpRpc backend);

}
