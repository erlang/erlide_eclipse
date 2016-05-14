package org.erlide.runtime;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.runtime.api.IOtpNodeProxy;
import org.erlide.runtime.api.RuntimeData;
import org.erlide.runtime.internal.OtpNodeProxy;

public class OtpNodeProxyProvider {

    @NonNull
    public static IOtpNodeProxy get(RuntimeData data) {
        return new OtpNodeProxy(data);
    }

}
