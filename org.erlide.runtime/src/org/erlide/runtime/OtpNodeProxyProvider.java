package org.erlide.runtime;

import org.erlide.runtime.api.IOtpNodeProxy;
import org.erlide.runtime.api.RuntimeData;
import org.erlide.runtime.internal.OtpNodeProxy;

public class OtpNodeProxyProvider {

    public static IOtpNodeProxy get(RuntimeData data) {
        return new OtpNodeProxy(data);
    }

}
