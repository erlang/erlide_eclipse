package org.erlide.engine;

import org.erlide.model.ErlModelException;
import org.erlide.model.erlang.ErlangToolkit;
import org.erlide.model.erlang.ErlangToolkitFactory;
import org.erlide.model.internal.root.ErlModel;
import org.erlide.model.root.IErlModel;
import org.erlide.model.services.search.ErlangXref;
import org.erlide.model.services.search.XrefService;
import org.erlide.util.ErlLogger;

public class ErlangEngineImpl implements IErlangServiceFactory {

    // !!!! XXX
    private volatile static IErlModel erlangModel;

    public ErlangEngineImpl() {
    }

    @Override
    public IErlModel getModel() {
        if (erlangModel == null) {
            final ErlangToolkit toolkit = ErlangToolkitFactory.getInstance();
            erlangModel = new ErlModel(toolkit);
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

    @Override
    public XrefService getXrefService() {
        return new ErlangXref();
    }

}
