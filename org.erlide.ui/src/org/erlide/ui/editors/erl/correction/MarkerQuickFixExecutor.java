package org.erlide.ui.editors.erl.correction;

import org.eclipse.jface.text.quickassist.IQuickAssistInvocationContext;

public class MarkerQuickFixExecutor extends QuickFixExecutor {

    @Override
    public boolean appliesAt(final IQuickAssistInvocationContext invocationContext) {
        return false;
    }

}
