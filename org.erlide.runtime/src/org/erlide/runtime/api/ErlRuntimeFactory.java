package org.erlide.runtime.api;

import org.erlide.runtime.internal.ErlRuntime;
import org.erlide.runtime.internal.NullErlRuntime;

public class ErlRuntimeFactory {

    public static IErlRuntime createRuntime(final RuntimeData data) {
        if (data == null) {
            return new NullErlRuntime();
        }
        return new ErlRuntime(data);
    }
}
