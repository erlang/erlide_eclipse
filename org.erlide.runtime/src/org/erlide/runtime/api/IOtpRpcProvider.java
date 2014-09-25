package org.erlide.runtime.api;

import org.erlide.runtime.runtimeinfo.RuntimeVersion;

public interface IOtpRpcProvider {

    IOtpRpc get();

    IOtpRpc get(RuntimeVersion version);

    IOtpRpc get(String projectName);

}
