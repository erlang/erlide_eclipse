package org.erlide.engine;

import org.erlide.model.root.IErlModel;
import org.erlide.model.services.search.OpenService;
import org.erlide.model.services.search.XrefService;

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

    public static synchronized ErlangEngine getInstance() {
        if (instance == null) {
            // TODO inject backend in factory
            instance = new ErlangEngine(ModelActivator.getErlangEngine());
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

    @Override
    public String getStateDir() {
        return factory.getStateDir();
    }

    @Override
    public OpenService getOpenService() {
        return factory.getOpenService();
    }

}
