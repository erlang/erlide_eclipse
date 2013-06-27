package org.erlide.engine;

import org.erlide.model.TextChange;

/**
 * Facade for the Erlang engine.
 * <p>
 * It serves as a single entry point to all functionality related to handling
 * Erlang code. This way it will be much easier to extract the engine and
 * implement it in Erlang or to let it be used by Xtext.
 * </p>
 */
public class ErlangEngine implements IEngineInput, IResourceChangeListener {

    @Override
    public void handleChangedInput(final String id, final TextChange change) {
    }

    @Override
    public void handleChangedResources(final ResourceChange delta) {

    }

}
