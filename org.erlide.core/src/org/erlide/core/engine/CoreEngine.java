package org.erlide.core.engine;

import org.erlide.engine.ErlangEngine;
import org.erlide.engine.ResourceChange;
import org.erlide.model.TextChange;

/**
 * Facade for the Erlang engine that is Eclipse-aware (uses Eclipse classes as
 * parameters).
 * <p>
 * It delegates to ErlideEngine after converting the values to/from non-Eclipse
 * ones.
 * </p>
 * 
 * @see ErlangEngine
 */
public class CoreEngine {

    public void handleChangedInput(final String id, final TextChange change) {
    }

    public void handleChangedResources(final ResourceChange delta) {

    }

}
