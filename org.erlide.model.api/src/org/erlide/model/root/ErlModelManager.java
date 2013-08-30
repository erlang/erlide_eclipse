package org.erlide.model.root;

import org.erlide.engine.IModelProvider;
import org.erlide.util.services.ExtensionUtils;

public class ErlModelManager {

    public static synchronized final IErlModel getErlangModel() {
        final IErlServiceProvider provider = ExtensionUtils
                .getSingletonExtension("org.erlide.model.api.serviceProvider",
                        IErlServiceProvider.class);
        return provider.get(IModelProvider.class).get();
    }

}
