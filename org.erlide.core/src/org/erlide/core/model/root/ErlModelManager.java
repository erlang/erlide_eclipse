package org.erlide.core.model.root;

import org.erlide.core.internal.model.root.ErlModel;
import org.erlide.core.model.erlang.ErlangToolkit;
import org.erlide.core.model.erlang.ErlangToolkitFactory;

public class ErlModelManager {
    private static volatile ErlModel fgErlangModel;

    public static synchronized final IErlModel getErlangModel() {
        if (fgErlangModel == null) {
            final ErlangToolkit toolkit = ErlangToolkitFactory.getInstance();
            fgErlangModel = new ErlModel(toolkit);
            fgErlangModel.buildStructure(null);
        }
        return fgErlangModel;
    }

}
