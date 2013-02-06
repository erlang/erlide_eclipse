package org.erlide.core.model.root;

import org.erlide.core.internal.model.root.ErlModel;
import org.erlide.core.model.erlang.ErlangToolkit;
import org.erlide.core.model.erlang.ErlangToolkitFactory;

public class ErlModelManager {
    private static volatile ErlModel erlangModel;

    public static synchronized final IErlModel getErlangModel() {
        if (erlangModel == null) {
            final ErlangToolkit toolkit = ErlangToolkitFactory.getInstance();
            erlangModel = new ErlModel(toolkit);
            erlangModel.buildStructure(null);
        }
        return erlangModel;
    }

}
