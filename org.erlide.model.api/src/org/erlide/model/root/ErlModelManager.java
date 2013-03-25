package org.erlide.model.root;

import org.erlide.model.ErlModelException;
import org.erlide.util.ErlLogger;
import org.erlide.util.ExtensionUtils;

public class ErlModelManager {
    private static volatile IErlModel erlangModel;

    public static synchronized final IErlModel getErlangModel() {
        if (erlangModel == null) {
            final IErlModelProvider provider = ExtensionUtils
                    .getSingletonExtension(
                            "org.erlide.model_api.modelProvider",
                            IErlModelProvider.class);
            erlangModel = provider.get();
        }
        if (!erlangModel.isOpen()) {
            try {
                erlangModel.open(null);
            } catch (final ErlModelException e) {
                ErlLogger.error(e);
            }
        }
        return erlangModel;
    }

}
