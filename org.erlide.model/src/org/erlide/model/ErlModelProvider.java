package org.erlide.model;

import org.erlide.model.erlang.ErlangToolkit;
import org.erlide.model.erlang.ErlangToolkitFactory;
import org.erlide.model.internal.root.ErlModel;
import org.erlide.model.root.IErlModel;
import org.erlide.model.root.IErlModelProvider;

public class ErlModelProvider implements IErlModelProvider {

    public ErlModelProvider() {
    }

    @Override
    public IErlModel get() {
        final ErlangToolkit toolkit = ErlangToolkitFactory.getInstance();
        return new ErlModel(toolkit);
    }

}
