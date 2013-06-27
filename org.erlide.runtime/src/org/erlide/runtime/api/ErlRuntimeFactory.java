package org.erlide.runtime.api;

import org.erlide.runtime.internal.ErlRuntime;
import org.erlide.runtime.internal.ManagedErlRuntime;
import org.erlide.runtime.internal.NullErlRuntime;

public class ErlRuntimeFactory {

    public static IErlRuntime createRuntime(final RuntimeData data) {
        if (data == null) {
            return new NullErlRuntime();
        }
        if (data.isManaged()) {
            return new ManagedErlRuntime(data);
        }
        return new ErlRuntime(data);
    }
}
