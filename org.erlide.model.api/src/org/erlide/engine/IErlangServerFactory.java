package org.erlide.engine;

import org.erlide.runtime.rpc.IOtpRpc;

public interface IErlangServerFactory {

    IErlangEngine get(IOtpRpc backend);

}
