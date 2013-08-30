package org.erlide.engine;

import org.erlide.model.ErlModelException;
import org.erlide.model.TextChange;
import org.erlide.model.erlang.ErlangToolkit;
import org.erlide.model.erlang.ErlangToolkitFactory;
import org.erlide.model.internal.root.ErlModel;
import org.erlide.model.root.IErlModel;
import org.erlide.util.ErlLogger;

/**
 * Facade for the Erlang engine.
 * <p>
 * It serves as a single entry point to all functionality related to handling
 * Erlang code. This way it will be much easier to extract the engine and
 * implement it in Erlang or to let it be used by Xtext.
 * </p>
 */
public class ErlangEngine implements IEngineInput, IResourceChangeListener,
        IModelProvider {

    private volatile static ErlangEngine instance;

    private volatile IErlModel erlangModel;

    private ErlangEngine() {
    }

    public static ErlangEngine getInstance() {
        if (instance == null) {
            instance = new ErlangEngine();
        }
        return instance;
    }

    @Override
    public IErlModel get() {
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
    public void handleChangedInput(final String id, final TextChange change) {
    }

    @Override
    public void handleChangedResources(final ResourceChange delta) {

    }

}
