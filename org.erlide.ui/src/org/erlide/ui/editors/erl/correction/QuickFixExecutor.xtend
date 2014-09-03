package org.erlide.ui.editors.erl.correction

import org.eclipse.core.resources.IMarker
import org.eclipse.core.runtime.ISafeRunnable
import org.eclipse.core.runtime.SafeRunner
import org.eclipse.jface.dialogs.MessageDialog
import org.eclipse.jface.text.quickassist.IQuickAssistInvocationContext
import org.eclipse.xtend.lib.annotations.Accessors
import org.erlide.util.ErlLogger

@Accessors
abstract class QuickFixExecutor implements ISafeRunnable {

    IMarker marker;
    QuickFix quickFix;

    override void handleException(Throwable exception) {
        ErlLogger.error(exception);
    }

    override void run() throws Exception {
        MessageDialog.openInformation(null, "Erlang quick fix", "This quick-fix is not yet implemented");
    }

    def void run(IMarker marker2, QuickFix erlangQuickFix) {
        setMarker(marker2);
        setQuickFix(erlangQuickFix);
        SafeRunner.run(this);
    }

    // only relevant for assists
    def boolean appliesAt(IQuickAssistInvocationContext invocationContext)

}
