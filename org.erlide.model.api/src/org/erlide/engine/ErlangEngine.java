package org.erlide.engine;

import org.erlide.model.root.IErlModel;
import org.erlide.model.services.search.XrefService;
import org.erlide.util.services.ExtensionUtils;

/**
 * Facade for the Erlang engine.
 * <p>
 * It serves as a single entry point to all functionality related to handling
 * Erlang code. This way it will be much easier to extract the engine and
 * implement it in Erlang or to let it be used by Xtext.
 * </p>
 */
public class ErlangEngine implements IErlangServiceFactory {
    private volatile static ErlangEngine instance;

    public static ErlangEngine getInstance() {
        if (instance == null) {

            // TODO inject backend in factory

            final IErlangServiceFactory afactory = ExtensionUtils
                    .getSingletonExtension(
                            "org.erlide.model.api.serviceFactory",
                            IErlangServiceFactory.class);
            instance = new ErlangEngine(afactory);
        }
        return instance;
    }

    private final IErlangServiceFactory factory;

    public ErlangEngine(final IErlangServiceFactory afactory) {
        factory = afactory;
    }

    @Override
    public IErlModel getModel() {
        return factory.getModel();
    }

    @Override
    public XrefService getXrefService() {
        return factory.getXrefService();
    }

}
