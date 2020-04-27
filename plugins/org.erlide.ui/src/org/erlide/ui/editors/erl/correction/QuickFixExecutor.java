package org.erlide.ui.editors.erl.correction;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.quickassist.IQuickAssistInvocationContext;
import org.erlide.util.ErlLogger;

@SuppressWarnings("all")
public abstract class QuickFixExecutor implements ISafeRunnable {
    private IMarker marker;

    private QuickFix quickFix;

    @Override
    public void handleException(final Throwable exception) {
        ErlLogger.error(exception);
    }

    @Override
    public void run() throws Exception {
        MessageDialog.openInformation(null, "Erlang quick fix",
                "This quick-fix is not yet implemented");
    }

    public void run(final IMarker marker2, final QuickFix erlangQuickFix) {
        setMarker(marker2);
        setQuickFix(erlangQuickFix);
        SafeRunner.run(this);
    }

    public abstract boolean appliesAt(
            final IQuickAssistInvocationContext invocationContext);

    public IMarker getMarker() {
        return marker;
    }

    public void setMarker(final IMarker marker) {
        this.marker = marker;
    }

    public QuickFix getQuickFix() {
        return quickFix;
    }

    public void setQuickFix(final QuickFix quickFix) {
        this.quickFix = quickFix;
    }
}
