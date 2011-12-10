package org.erlide.debug.ui.tracing;

import org.eclipse.debug.core.model.DebugElement;
import org.eclipse.debug.core.model.IDebugTarget;

public class DebugTraceElement extends DebugElement {

    public DebugTraceElement(final IDebugTarget target) {
        super(target);
    }

    @Override
    public String getModelIdentifier() {
        return DebugTraceDebugModelPresentation.ID;
    }

}
