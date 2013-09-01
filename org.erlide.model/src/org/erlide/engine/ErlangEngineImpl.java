package org.erlide.engine;

import org.eclipse.core.runtime.Platform;
import org.erlide.model.ErlModelException;
import org.erlide.model.erlang.ErlangToolkit;
import org.erlide.model.erlang.ErlangToolkitFactory;
import org.erlide.model.internal.root.ErlModel;
import org.erlide.model.root.IErlModel;
import org.erlide.model.services.search.ErlangXref;
import org.erlide.model.services.search.ErlideDoc;
import org.erlide.model.services.search.ErlideOpen;
import org.erlide.model.services.search.OpenService;
import org.erlide.model.services.search.OtpDocService;
import org.erlide.model.services.search.XrefService;
import org.erlide.util.ErlLogger;
import org.osgi.framework.Bundle;

public class ErlangEngineImpl implements IErlangServiceFactory {

    public ErlangEngineImpl() {

        // TODO how to inject runtime and other start params?

    }

    private volatile static IErlModel erlangModel;

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

    private volatile String stateDirCached;

    @Override
    public String getStateDir() {
        if (stateDirCached == null) {
            final Bundle modelPlugin = Platform.getBundle("org.erlide.model");
            stateDirCached = Platform.getStateLocation(modelPlugin)
                    .toPortableString();
        }
        return stateDirCached;
    }

    @Override
    public OpenService getOpenService() {
        return new ErlideOpen();
    }

    @Override
    public OtpDocService getOtpDocService() {
        return new ErlideDoc();
    }
}
