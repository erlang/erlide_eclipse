package org.erlide.ui.editors.erl.correction;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.jface.dialogs.MessageDialog;
import org.erlide.util.ErlLogger;

public abstract class ErlangQuickFixRunnable implements ISafeRunnable {

    private IMarker marker;
    private ErlangQuickFix fix;

    @Override
    public void handleException(final Throwable exception) {
        ErlLogger.error(exception);
    }

    @Override
    public void run() throws Exception {
        MessageDialog.openInformation(null, "Erlang quick fix",
                "This quick-fix is not yet implemented");
    }

    public IMarker getMarker() {
        return marker;
    }

    public void setMarker(final IMarker marker) {
        this.marker = marker;
    }

    public void setQuickFix(final ErlangQuickFix fix) {
        this.fix = fix;
    }

    public ErlangQuickFix getQuickFix() {
        return fix;
    }
}
