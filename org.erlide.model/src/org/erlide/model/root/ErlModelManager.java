package org.erlide.model.root;

import org.erlide.model.erlang.ErlangToolkit;
import org.erlide.model.erlang.ErlangToolkitFactory;
import org.erlide.model.internal.root.ErlModel;

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
