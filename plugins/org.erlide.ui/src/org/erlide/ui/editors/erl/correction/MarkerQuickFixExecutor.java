package org.erlide.ui.editors.erl.correction;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.text.quickassist.IQuickAssistInvocationContext;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.root.IErlModel;
import org.erlide.engine.model.root.IErlModule;

public class MarkerQuickFixExecutor extends QuickFixExecutor {

    protected IErlModule module;

    @Override
    public void setMarker(final IMarker marker) {
        super.setMarker(marker);

        final IErlModel model = ErlangEngine.getInstance().getModel();
        module = model.findModule((IFile) marker.getResource());
    }

    @Override
    public boolean appliesAt(final IQuickAssistInvocationContext invocationContext) {
        return false;
    }

}
