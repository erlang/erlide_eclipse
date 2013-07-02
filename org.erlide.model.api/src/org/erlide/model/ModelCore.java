package org.erlide.model;

import org.eclipse.core.runtime.Platform;
import org.osgi.framework.Bundle;

public class ModelCore {
    private static String stateDirCached = null;

    public static String getStateDir() {
        if (stateDirCached == null) {
            final Bundle modelPlugin = Platform.getBundle("org.erlide.model");
            stateDirCached = Platform.getStateLocation(modelPlugin)
                    .toPortableString();
        }
        return stateDirCached;
    }

}
