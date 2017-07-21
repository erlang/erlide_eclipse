package org.erlide.runtime.rpc;

import org.erlide.runtime.runtimeinfo.RuntimeVersion;

public interface IOtpRpcProvider {

    IOtpRpc get();

    IOtpRpc get(RuntimeVersion version);

    IOtpRpc get(String projectName);

}
