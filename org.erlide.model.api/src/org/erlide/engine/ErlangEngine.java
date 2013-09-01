package org.erlide.engine;

import org.erlide.model.root.IErlModel;
import org.erlide.model.services.search.XrefService;
import org.erlide.util.services.ExtensionUtils;

public class ErlangEngine implements IErlangServiceFactory {
    private volatile static ErlangEngine instance;

    public static ErlangEngine getInstance() {
        if (instance == null) {
            instance = new ErlangEngine();
        }
        return instance;
    }

    @Override
    public IErlModel getModel() {
        final IErlangServiceFactory factory = ExtensionUtils
                .getSingletonExtension("org.erlide.model.api.serviceFactory",
                        IErlangServiceFactory.class);
        return factory.getModel();
    }

    @Override
    public XrefService getXrefService() {
        final IErlangServiceFactory factory = ExtensionUtils
                .getSingletonExtension("org.erlide.model.api.serviceFactory",
                        IErlangServiceFactory.class);
        return factory.getXrefService();
    }

}
