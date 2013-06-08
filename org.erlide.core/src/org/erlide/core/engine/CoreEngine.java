package org.erlide.core.engine;

import org.eclipse.core.resources.IResourceDelta;
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

    private final ErlangEngine engine;

    public CoreEngine(final ErlangEngine engine) {
        this.engine = engine;
    }

    public void handleChangedInput(final String id, final TextChange change) {
        engine.handleChangedInput(id, change);
    }

    public void handleChangedResources(final IResourceDelta delta) {
        engine.handleChangedResources(createResourceChange(delta));
    }

    private ResourceChange createResourceChange(final IResourceDelta delta) {
        return null;
    }

}
