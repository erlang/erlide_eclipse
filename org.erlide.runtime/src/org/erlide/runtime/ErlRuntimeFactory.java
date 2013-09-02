package org.erlide.runtime;

import org.eclipse.jdt.annotation.NonNull;
import org.erlide.runtime.api.IErlRuntime;
import org.erlide.runtime.api.RuntimeData;
import org.erlide.runtime.internal.ErlRuntime;
import org.erlide.runtime.internal.ManagedErlRuntime;
import org.erlide.runtime.internal.NullErlRuntime;

public class ErlRuntimeFactory {

    @NonNull
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
